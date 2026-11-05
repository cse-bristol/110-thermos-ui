;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns geometry.core
  "The part of clj-geometry's geometry.core which geometry.gpkg uses,
  for when JTS is too old for the real one. thermos-backend.gpkg.old-jts
  loads this; nothing else should."
  (:import [org.locationtech.jts.geom Geometry GeometryFactory]))

(def ^:dynamic ^GeometryFactory *factory* (GeometryFactory.))

(defprotocol HasGeometry
  (geometry ^org.locationtech.jts.geom.Geometry [g])
  (update-geometry [x g]))

(extend-type Geometry HasGeometry
             (geometry [g] g)
             (update-geometry [_ g] g))

(defn srid [g] (.getSRID (geometry g)))
