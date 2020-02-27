CREATE OR REPLACE FUNCTION map_icon (_map_id int)
RETURNS bytea
AS $$
  WITH geoms AS
  (SELECT geometry FROM candidates INNER JOIN paths ON candidates.id = paths.candidate_id
  WHERE map_id = _map_id)
  SELECT ST_AsPNG(
         ST_AsRaster(
         ST_ConcaveHull(ST_Collect(geometry), 0.75),
         100, 100,
         ARRAY['8BUI', '8BUI', '8BUI'],
         ARRAY[230,100,100],
         ARRAY[255,255,255])) png
  FROM geoms
  ORDER BY random()
  LIMIT 1500;
$$ LANGUAGE sql;
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
     -- try 3 times, since this is randomized and sometimes fails.
     BEGIN
     INSERT INTO map_icons (map_id, png) values (_map_id, map_icon(_map_id));
     EXCEPTION WHEN OTHERS THEN BEGIN
     INSERT INTO map_icons (map_id, png) values (_map_id, map_icon(_map_id));
     EXCEPTION WHEN OTHERS THEN BEGIN
     INSERT INTO map_icons (map_id, png) values (_map_id, map_icon(_map_id));
     EXCEPTION WHEN OTHERS THEN
     -- doesn't matter in the end if we get no icon I guess
     END;
     END;
     END;

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
