ALTER TABLE maps
   ADD COLUMN estimation_stats json;
--;;
UPDATE maps SET estimation_stats =
  (SELECT json_object_agg(demand_source, count) FROM
     (SELECT demand_source, count(*) count
      FROM buildings INNER JOIN candidates ON buildings.candidate_id = candidates.id
      WHERE map_id = maps.id GROUP BY demand_SOURCE) AS demand_sources);
--;;
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

     DELETE FROM map_icons WHERE map_id = _map_id;
     INSERT INTO map_icons (map_id, png)
            values (_map_id, map_icon(_map_id));

     UPDATE maps SET estimation_stats = 
      (SELECT json_object_agg(demand_source, count) FROM
        (SELECT coalesce(demand_source, 'unknown'), count(*) count
         FROM buildings INNER JOIN candidates ON buildings.candidate_id = candidates.id
         WHERE map_id = _map_id GROUP BY demand_SOURCE) AS demand_sources);
     
     DELETE FROM tilecache WHERE map_id = _map_id;
     DELETE FROM tilemaxima WHERE map_id = _map_id;
   END;
$$ LANGUAGE plpgsql;
--;;
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

     DELETE FROM map_icons WHERE map_id = _map_id;
     INSERT INTO map_icons (map_id, png)
            values (_map_id, map_icon(_map_id));

     UPDATE maps SET estimation_stats = 
      (SELECT json_object_agg(demand_source, count) FROM
        (SELECT demand_source, count(*) count
         FROM buildings INNER JOIN candidates ON buildings.candidate_id = candidates.id
         WHERE map_id = _map_id GROUP BY demand_SOURCE) AS demand_sources)
       WHERE map_id = _map_id;
     
     DELETE FROM tilecache WHERE map_id = _map_id;
     DELETE FROM tilemaxima WHERE map_id = _map_id;
   END;
$$ LANGUAGE plpgsql;
