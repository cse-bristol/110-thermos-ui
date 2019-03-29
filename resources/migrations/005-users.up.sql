CREATE TABLE projects (
   id serial not null primary key,
   name text not null,
   description text not null
)
--;;
CREATE TABLE maps (
   id serial not null primary key,
   project_id integer
      references projects(id)
      on delete cascade,
   name text not null,
   import_completed boolean default false,
   job_id int references jobs(id) on delete set null
)
--;;
CREATE TYPE user_auth AS ENUM ( 'normal', 'admin' )
--;;
CREATE TABLE users (
   id text not null primary key,   -- email address
   name text not null, -- display name
   auth user_auth not null, -- user type
   password text,
   reset_token text
)
--;;
CREATE TYPE project_auth AS ENUM ( 'read', 'write', 'admin' )
--;;
CREATE TABLE users_projects (
   project_id INTEGER REFERENCES projects(id) on delete cascade,
   user_id text references users(id) on delete cascade,
   auth project_auth not null, -- what authority user has over project
   primary key (project_id, user_id)
)
--;;
ALTER TABLE candidates
ADD COLUMN map_id INTEGER
    REFERENCES maps (id)
    ON DELETE CASCADE;
--;;
DROP TABLE problems;
--;;
CREATE TABLE networks (
   id serial not null primary key,
   map_id integer references maps(id) on delete cascade,
   name text,
   content text not null,
   created timestamp without time zone default (now() at time zone 'utc'),
   has_run boolean not null default false,
   job_id integer references jobs(id) on delete set null
);
--;;
DROP VIEW joined_candidates;
--;;
CREATE VIEW joined_candidates AS
SELECT
        candidates.id as id,
        candidates.map_id as map_id,
        candidates.name as name,
        candidates.type as type,
        candidates.geometry as raw_geometry,
        ST_AsGeoJson(candidates.geometry) as geometry,

        buildings.id is not null as is_building,
        
        array_to_string(buildings.connection_id, ',') as connection_ids,
        buildings.demand_kwh_per_year as demand_kwh_per_year,
        buildings.demand_kwp as demand_kwp,
        buildings.connection_count as connection_count,

        paths.start_id as start_id,
        paths.end_id as end_id,
        paths.length as length,
        paths.unit_cost as unit_cost
FROM
        candidates
        LEFT JOIN buildings on candidates.id = buildings.id
        LEFT JOIN paths on candidates.id = paths.id
;
--;;
ALTER TABLE tilecache
ADD COLUMN map_id INTEGER
    REFERENCES maps (id)
    ON DELETE CASCADE;
--;;
DROP TABLE tilemaxima;
--;;
CREATE TABLE tilemaxima (
       z int not null,
       map_id int not null,
       maximum real not null,
       primary key (z, map_id)
);
--;;
CREATE OR REPLACE VIEW heat_centroids AS
SELECT
        candidates.geometry as geometry,
        ST_X(ST_Centroid(candidates.geometry)) as x,
        ST_Y(ST_Centroid(candidates.geometry)) as y,
        buildings.demand_kwh_per_year as demand,
        map_id as map_id
FROM
        candidates
        INNER JOIN buildings ON candidates.id = buildings.id
;
--;;
CREATE OR REPLACE VIEW ranked_jobs AS
SELECT id,state,queue_name,
       row_number()
        OVER (PARTITION BY queue_name,
              state in ('ready', 'running', 'cancel', 'cancelling')
              ORDER BY queued)
       as queue_position
FROM jobs;
--;;
CREATE TABLE map_centres AS
SELECT map_id, st_x(c) x, st_y(c) y FROM
( SELECT map_id, ST_Centroid(ST_Envelope(ST_Collect(geometry))) c
  FROM candidates INNER JOIN buildings on candidates.id = buildings.id
  GROUP BY map_id
) centroids;
--;;
CREATE TABLE map_icons AS
WITH geoms AS
(SELECT map_id, geometry FROM candidates INNER JOIN paths ON candidates.id = paths.id)
SELECT ST_AsPNG(
       ST_AsRaster(
       ST_ConcaveHull(ST_Collect(geometry), 0.75),
       100, 100,
       ARRAY['8BUI', '8BUI', '8BUI'],
       ARRAY[230,100,100],
       ARRAY[255,255,255])) png, map_id
FROM geoms
WHERE random() < 0.1
GROUP BY map_id
--;;
CREATE OR REPLACE FUNCTION map_icon (_map_id int)
RETURNS bytea
AS $$

  WITH geoms AS
  (SELECT geometry FROM candidates INNER JOIN paths ON candidates.id = paths.id
  WHERE map_id = _map_id)
  SELECT ST_AsPNG(
         ST_AsRaster(
         ST_ConcaveHull(ST_Collect(geometry), 0.75),
         100, 100,
         ARRAY['8BUI', '8BUI', '8BUI'],
         ARRAY[230,100,100],
         ARRAY[255,255,255])) png
  FROM geoms
  WHERE random() < 0.1;

$$ LANGUAGE sql;
--;;
CREATE OR REPLACE FUNCTION update_map (_map_id int)
RETURNS void
AS $$
   BEGIN
     DELETE FROM map_centres WHERE map_id = _map_id;
     INSERT INTO map_centres
     (SELECT map_id, st_x(c), st_y(c) FROM
      (SELECT map_id, ST_Centroid(ST_Envelope(ST_Collect(geometry))) c
       FROM candidates INNER JOIN buildings on candidates.id = buildings.id
       WHERE map_id = _map_id
       GROUP BY map_id
       ) a);

     DELETE FROM map_icons WHERE map_id = _map_id;
     INSERT INTO map_icons (map_id, png)
            values (_map_id, map_icon(_map_id));

     DELETE FROM tilecache WHERE map_id = _map_id;
     DELETE FROM tilemaxima WHERE map_id = _map_id;
   END;
$$ LANGUAGE plpgsql;
