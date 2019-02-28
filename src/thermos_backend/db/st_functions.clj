(ns thermos-backend.db.st-functions
  (:require [honeysql.core :as sql]))

(defmacro defst [fname & args]
  `(defn ~fname
     ~@(for [arity args]
         `(~arity
           (sql/call ~(keyword (str "ST_" (.replace (name fname)
                                                    \- \_)))
                     ~@arity)))))

(defst y [geom])
(defst x-min [geom])
(defst y-min [geom])
(defst x-max [geom])
(defst y-max [geom])
(defst aspng [raster])
(defst collect [geoms])
(defst asraster [geom width height pixel-type raster-color empty-color])
(defst geomfromtext
  [text]
  [text srid])
(defst geogfromtext [text])
(defst buffer [geom rad])
(defn geometry [thing] (sql/call :geometry thing))
(defst convexhull [geometry])
(defst concavehull
  [geometry target-percent]
  [geometry target-percent allow-holes])
