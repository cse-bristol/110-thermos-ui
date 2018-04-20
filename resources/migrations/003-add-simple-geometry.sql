CREATE INDEX simple_building_geometry
ON buildings(ST_SimplifyPreserveTopology(geometry, 0.0001));
--;;
CREATE INDEX simple_way_geometry
ON ways(ST_SimplifyPreserveTopology(geometry, 0.0001));
