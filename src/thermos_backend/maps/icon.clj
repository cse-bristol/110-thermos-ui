;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.maps.icon
  "A map's icon: a little picture of it, with its paths and buildings
  over a glow of heat demand coloured like the editor's heat map, so
  you can see what the map covers and where the demand is.

  Icons are drawn with Java2D and kept in map_icons. They are made
  after an import (see db.maps/insert-into-map!), or when one is wanted
  which isn't there (`get-or-render!`)."
  (:require [thermos-backend.db :as db]
            [thermos-backend.maps.heat-density :as heat-density]
            [clojure.tools.logging :as log])
  (:import [java.awt BasicStroke Color RenderingHints]
           [java.awt.geom AffineTransform Ellipse2D$Double Point2D$Double]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [org.apache.commons.math3.linear Array2DRowRealMatrix]
           [org.locationtech.jts.awt ShapeWriter]
           [org.locationtech.jts.geom Geometry]
           [org.locationtech.jts.geom.util AffineTransformation]
           [org.locationtech.jts.io WKBReader]))

(def size
  "The width and height of an icon in pixels. Pages show it at half
  this, so it's sharp on high resolution screens."
  192)

(def ^:private padding 8)

(def ^:private density-bandwidth
  "The radius in pixels of the blur which makes demand into a glow"
  12)

(def ^:private background (Color. 0xf3 0xf4 0xf1))
(def ^:private path-colour (Color. 0xa4 0xaa 0xb0))
(def ^:private building-colour (Color. 0x6b 0x71 0x78 180))

(def ^:private writer
  "Makes JTS geometry into Java2D shapes"
  (ShapeWriter.))

(defn- extent
  "The [min-x min-y max-x max-y] of the candidates on map `map-id`, or
  nil if it has none"
  [map-id]
  (let [{:keys [x1 y1 x2 y2]}
        (db/fetch-one!
         ["SELECT ST_XMin(e) x1, ST_YMin(e) y1, ST_XMax(e) x2, ST_YMax(e) y2
           FROM (SELECT ST_Extent(geometry) e FROM candidates WHERE map_id = ?) a"
          map-id])]
    (when x1 [x1 y1 x2 y2])))

(defn- features
  "The candidates on map `map-id`, simplified to about half a pixel at
  `degrees-per-pixel`, as maps with a JTS :geometry, whether it :is-path,
  and a building's annual heat (or if it has none, cooling) :demand"
  [map-id degrees-per-pixel]
  (let [reader (WKBReader.)]
    (for [{:keys [g demand cooling is-path]}
          (db/fetch!
           ["SELECT ST_AsBinary(ST_Simplify(candidates.geometry, ?, true)) g,
                    buildings.demand_kwh_per_year demand,
                    buildings.cooling_kwh_per_year cooling,
                    paths.candidate_id IS NOT NULL is_path
             FROM candidates
             LEFT JOIN buildings ON buildings.candidate_id = candidates.id
             LEFT JOIN paths ON paths.candidate_id = candidates.id
             WHERE candidates.map_id = ?"
            (/ degrees-per-pixel 2.0) map-id])
          :when g]
      {:geometry (.read reader ^bytes g)
       :is-path  is-path
       :demand   (if (and demand (pos? demand)) demand (or cooling 0))})))

(defn- transformation
  "The transformation from lon/lat to pixels which fits `extent` in the
  icon, keeping its shape (squashing longitude by the cosine of the
  latitude, which is near enough at this size), and centred"
  ^AffineTransform [[x1 y1 x2 y2]]
  (let [k     (Math/cos (Math/toRadians (/ (+ y1 y2) 2.0)))
        w     (* k (- x2 x1))
        h     (- y2 y1)
        scale (/ (- size padding padding) (max w h 1e-9))
        ox    (/ (- size (* scale w)) 2.0)
        oy    (/ (- size (* scale h)) 2.0)]
    ;; x' = ox + scale k (x - x1), and y goes down in the image, so
    ;; y' = size - oy - scale (y - y1)
    (AffineTransform. (* scale k) 0.0
                      0.0 (- scale)
                      (- ox (* scale k x1)) (+ (- size oy) (* scale y1)))))

(defn- to-pixel
  "The pixel [x y] of lon/lat `x` `y`"
  [^AffineTransform t x y]
  (let [p (.transform t (Point2D$Double. (double x) (double y)) nil)]
    [(.getX p) (.getY p)]))

(defn- density-image
  "An image of the glow of `demands`, which are [x y kWh] in pixels,
  coloured like the editor's heat map"
  ^BufferedImage [demands]
  (let [b      density-bandwidth
        full   (+ size b b)
        raster (Array2DRowRealMatrix. (int full) (int full))]
    (doseq [[x y demand] demands
            :let [i (+ b (int x)) j (+ b (int y))]
            :when (and (< -1 i full) (< -1 j full))]
      (.addToEntry raster i j demand))
    (let [blurred (heat-density/blur-raster raster b)
          values  (for [i (range size) j (range size)]
                    (.getEntry blurred (+ i b) (+ j b)))
          ;; scale so a few of the brightest pixels are red, rather than
          ;; one big building making everything else faint
          top     (let [v (vec (sort (filter pos? values)))]
                    (when (seq v) (nth v (int (* 0.98 (dec (count v)))))))
          image   (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)]
      (when (and top (pos? top))
        (doseq [i (range size) j (range size)
                :let [v (min 1.0 (/ (.getEntry blurred (+ i b) (+ j b)) top))]
                :when (pos? v)]
          (let [c (int (* 255 v))]
            (.setRGB image i j
                     (unchecked-int
                      (bit-or (bit-shift-left (int (* 220 (Math/pow v 1.5))) 24)
                              (bit-shift-left c 16)
                              (bit-shift-left (int (/ (- 255 c) 1.7)) 8)
                              (- 255 c)))))))
      image)))

(defn render
  "A PNG of map `map-id`, or nil if it has no candidates"
  ^bytes [map-id]
  (when-let [[x1 y1 x2 y2 :as box] (extent map-id)]
    (let [t         (transformation box)
          px        (fn [x y] (to-pixel t x y))
          ;; into pixels before shapes, which have floats
          jts-t     (AffineTransformation. (.getScaleX t) (.getShearX t) (.getTranslateX t)
                                           (.getShearY t) (.getScaleY t) (.getTranslateY t))
          shape     (fn [^Geometry g] (.toShape writer (.transform jts-t g)))
          [px1 _]   (px x1 y1)
          [px2 _]   (px x2 y1)
          degrees-per-pixel (/ (- x2 x1) (max 1e-9 (- px2 px1)))
          features  (features map-id degrees-per-pixel)
          image     (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
          g         (.createGraphics image)]
      (doto g
        (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        (.setColor background)
        (.fillRect 0 0 size size)
        (.setColor path-colour)
        (.setStroke (BasicStroke. 0.8 BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND)))

      (doseq [{:keys [^Geometry geometry is-path]} features :when is-path]
        (.draw g (shape geometry)))
      (.setColor g building-colour)
      (doseq [{:keys [^Geometry geometry is-path]} features
              :when (not is-path)]
        (if (zero? (.getDimension geometry))
          (let [[x y] (px (.getX (.getCentroid geometry)) (.getY (.getCentroid geometry)))]
            (.fill g (Ellipse2D$Double. (- x 1.5) (- y 1.5) 3 3)))
          (.fill g (shape geometry))))
      ;; the demand goes on top, so the hot spots stand out
      (.drawImage g (density-image
                     (for [{:keys [^Geometry geometry is-path demand]} features
                           :when (and (not is-path) (pos? demand))
                           :let [c (.getCentroid geometry)]
                           :when (not (.isEmpty c))]
                       (conj (px (.getX c) (.getY c)) demand)))
                  0 0 nil)
      (.dispose g)

      (let [out (java.io.ByteArrayOutputStream.)]
        (ImageIO/write image "png" out)
        (.toByteArray out)))))

(defn store!
  "Draw the icon of map `map-id` again, and keep it"
  [map-id]
  (try
    (when-let [png (render map-id)]
      (db/execute!
       ["INSERT INTO map_icons (map_id, png) VALUES (?, ?)
         ON CONFLICT (map_id) DO UPDATE SET png = EXCLUDED.png"
        map-id png])
      png)
    (catch Exception e
      ;; a map without an icon is no great loss
      (log/error e "Drawing the icon for map" map-id))))

(defn get-or-render!
  "The icon of map `map-id`, drawing it if it hasn't got one"
  [map-id]
  (or (:png (db/fetch-one! ["SELECT png FROM map_icons WHERE map_id = ?" map-id]))
      (store! map-id)))
