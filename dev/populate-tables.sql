-- populate the backend db tables from the old data we have imported
-- TODO are the backend tables how they ought to be?

CREATE TABLE demand AS
SELECT id, name, address, postcode, demand, wkb_geometry as geometry
FROM old_demand;
