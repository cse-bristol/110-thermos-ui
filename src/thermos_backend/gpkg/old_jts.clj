;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.gpkg.old-jts
  "A workaround, so that clj-geometry's geometry.gpkg works with JTS 1.16.

  geometry.gpkg needs very little of geometry.core: *factory*, srid and
  the HasGeometry protocol. But geometry.core imports classes which are
  new in JTS 1.18 and 1.19 (GeometryFixer, OverlayNGRobust, ConcaveHull),
  and GeoTools 20, which thermos-importer uses, doesn't work with any JTS
  after 1.16 (it calls Polygon.getExteriorRing, whose return type changed
  in 1.17, so writing geojson and reprojecting fail). So deps.edn keeps
  JTS at 1.16, and if that is too old for geometry.core, this loads
  geometry_core.clj, which defines (as geometry.core) the part of it
  geometry.gpkg uses, so geometry.gpkg doesn't load the real one.

  When thermos-importer has a newer GeoTools, or geometry.gpkg no longer
  needs geometry.core, this can go.")

(defn- has-class? [name]
  (try (Class/forName name) true
       (catch ClassNotFoundException _ false)))

(when-not (or (find-ns 'geometry.core)
              (has-class? "org.locationtech.jts.algorithm.hull.ConcaveHull"))
  ;; its ns form marks geometry.core as loaded, so (require 'geometry.core)
  ;; doesn't load the real one
  (load "geometry_core"))
