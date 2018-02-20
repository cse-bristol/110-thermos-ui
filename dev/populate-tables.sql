-- populate the backend db tables from the old data we have imported
-- TODO are the backend tables how they ought to be?

-- Buildings:
-- id
-- geometry
-- type
-- demand?
-- name & address
-- postcode
-- building type

-- Roads:
-- id
-- name
-- postcode
-- geometry
-- length (maybe)
-- from-id
-- to-id

ALTER TABLE old_demand ADD COLUMN new_id CHAR(32);
ALTER TABLE old_connections ADD COLUMN new_id CHAR(32);
-- generate IDs from geometries:

UPDATE old_demand SET new_id = md5(st_astext(wkb_geometry));
UPDATE old_connections SET new_id = md5(st_astext(wkb_geometry));

UPDATE old_connections SET node_from = old_connections.new_id
FROM old_demand where old_connections.node_from = old_demand.id;

UPDATE old_connections SET node_to = old_connections.new_id
FROM old_demand where old_connections.node_to = old_demand.id;

-- construct our two tables
CREATE TABLE buildings AS
SELECT
  new_id as id,
  (name || address) as name,
  'demand' as type,
  'unknown' as building_type,
  postcode,
  demand,
  wkb_geometry as geometry
FROM old_demand;

CREATE TABLE ways AS
SELECT
  new_id as id,
  name,
  'BS3 1ED' as postcode,
  length,
  node_from,
  node_to,
  wkb_geometry as geometry
FROM old_connections;
