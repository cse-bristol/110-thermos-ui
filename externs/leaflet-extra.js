// these are some extra externs currently missing from leaflet,
// required to allow advanced compilation to work.
// I (Tom) produced these by some trial and error.

L.Point.multiplyBy = function(scale) {};
L.Point.scaleBy = function(scale) {};
L.Point.unscaleBy = function(scale) {};
L.Point.subtract = function(scale) {};
L.GridLayer.getTileSize = function() {};
L.map.flyToBounds = function(bounds) {};
L.Map.flyToBounds = function(bounds) {};
L.Layer.toGeoJSON = function() {};
