DROP INDEX IF EXISTS simple_geometry_index;
--;;
DROP VIEW IF EXISTS joined_candidates;
--;;
ALTER TABLE candidates ADD COLUMN simple_geometry geometry(GEOMETRY, 4326);
--;;
UPDATE candidates SET simple_geometry = ST_SimplifyPreserveTopology(geometry, 0.0001);
--;;
CREATE OR REPLACE FUNCTION update_simple_geometry() RETURNS trigger AS
$$
BEGIN
NEW.simple_geometry = ST_SimplifyPreserveTopology(NEW.geometry, 0.0001);
return NEW;
END
$$ LANGUAGE plpgsql;
--;;
CREATE TRIGGER update_simple_geometry AFTER INSERT OR UPDATE OF geometry ON candidates
FOR EACH ROW
EXECUTE PROCEDURE update_simple_geometry();
--;;
CREATE VIEW joined_candidates AS
SELECT candidates.id as id, name, "type", subtype,
array_to_string(connection_id, ',') as connection_id,
demand, start_id, end_id,
candidates.geometry as real_geometry,
ST_AsGeoJSON(geometry) as geometry,
ST_AsGeoJSON(simple_geometry) as simple_geometry
FROM candidates
LEFT JOIN buildings ON candidates.id = buildings.id
LEFT JOIN paths on candidates.id = paths.id;
