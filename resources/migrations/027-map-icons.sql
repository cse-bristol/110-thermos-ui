-- Map icons are now drawn in Clojure (thermos-backend.maps.icon), after
-- update_map, so it no longer makes them.
CREATE OR REPLACE FUNCTION update_map (_map_id int)
RETURNS void
AS $$
   BEGIN
     DELETE FROM map_centres WHERE map_id = _map_id;
     INSERT INTO map_centres
     (SELECT map_id, envelope FROM
      (SELECT map_id, ST_Envelope(ST_Collect(geometry)) envelope
       FROM candidates INNER JOIN buildings on candidates.id = buildings.candidate_id
       WHERE map_id = _map_id
       GROUP BY map_id
       ) a);

     UPDATE maps SET estimation_stats =
      (SELECT json_object_agg(demand_source, count) FROM
        (SELECT demand_source, count(*) count
         FROM buildings INNER JOIN candidates ON buildings.candidate_id = candidates.id
         WHERE map_id = _map_id GROUP BY demand_SOURCE) AS demand_sources)
       WHERE id = _map_id;

     DELETE FROM tilecache WHERE map_id = _map_id;
     DELETE FROM tilemaxima WHERE map_id = _map_id;
   END;
$$ LANGUAGE plpgsql;
--;;
DROP FUNCTION IF EXISTS map_icon(int);
--;;
-- the old icons are redrawn in the new style when they're next wanted
DELETE FROM map_icons;
--;;
CREATE UNIQUE INDEX IF NOT EXISTS map_icons_map_id_key ON map_icons (map_id);
