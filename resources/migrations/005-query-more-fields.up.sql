DROP VIEW IF EXISTS joined_candidates;
--;;
CREATE VIEW joined_candidates AS
SELECT candidates.id as id, name, "type", subtype,
array_to_string(connection_id, ',') as connection_id,
demand, "length", cost, start_id, end_id,
candidates.geometry as real_geometry,
ST_AsGeoJSON(geometry) as geometry,
ST_AsGeoJSON(simple_geometry) as simple_geometry
FROM candidates
LEFT JOIN buildings ON candidates.id = buildings.id
LEFT JOIN paths on candidates.id = paths.id;
