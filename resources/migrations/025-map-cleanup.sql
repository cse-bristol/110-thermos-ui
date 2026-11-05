CREATE INDEX IF NOT EXISTS candidates_map_id_idx ON candidates (map_id);
--;;
DELETE FROM map_centres mc WHERE NOT EXISTS (SELECT 1 FROM maps m WHERE m.id = mc.map_id);
--;;
DELETE FROM map_icons mi WHERE NOT EXISTS (SELECT 1 FROM maps m WHERE m.id = mi.map_id);
--;;
ALTER TABLE map_centres ADD CONSTRAINT map_centres_map_id_fkey
   FOREIGN KEY (map_id) REFERENCES maps(id) ON DELETE CASCADE;
--;;
ALTER TABLE map_icons ADD CONSTRAINT map_icons_map_id_fkey
   FOREIGN KEY (map_id) REFERENCES maps(id) ON DELETE CASCADE;
