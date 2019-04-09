CREATE TYPE job_state AS ENUM (
  'ready', 'running', 'completed', 'cancel', 'cancelling', 'cancelled', 'failed'
)
--;; 1
CREATE TABLE jobs (
    id serial not null primary key,
    queue_name text not null,
    args text not null,
    state job_state not null default 'ready',
    queued timestamp without time zone default (now() at time zone 'utc'),
    updated timestamp without time zone default (now() at time zone 'utc')
);
--;; 2
CREATE INDEX job_state_idx on jobs (state, id);
--;; 3
CREATE INDEX job_state_queue_idx on jobs (state, queue_name ,id);
--;; 4
CREATE OR REPLACE FUNCTION update_changetimestamp_column()
RETURNS TRIGGER AS $$
BEGIN
   NEW.updated = now(); 
   RETURN NEW;
END;
$$ language 'plpgsql';
--;; 5
CREATE TRIGGER update_jobs_changetimestamp BEFORE UPDATE
ON jobs FOR EACH ROW EXECUTE PROCEDURE 
update_changetimestamp_column();
--;; 6
CREATE TABLE projects (
   id serial not null primary key,
   name text not null,
   description text not null
)
--;; 7
CREATE TABLE maps (
   id serial not null primary key,
   project_id integer
      references projects(id)
      on delete cascade,
   name text not null,
   parameters text not null, -- EDN
   import_completed boolean default false,
   job_id int references jobs(id) on delete set null
)
--;; 8
CREATE TABLE candidates (
   id varchar(40) not null primary key,
   orig_id text not null,
   name text not null,
   type text not null,
   geometry geometry(GEOMETRY, 4326) not null,
   map_id integer not null references maps(id)
)
--;; 9
CREATE TABLE buildings (
   id text not null primary key
     references candidates(id) on delete cascade,
   connection_id text[] not null,
   demand_kwh_per_year real not null,
   demand_kwp real not null,
   connection_count int not null,
   connection_cost real not null
)
--;; 10
CREATE TABLE paths (
   id text not null primary key
     references candidates(id) on delete cascade,
   start_id text not null,
   end_id text not null,
   length real not null,
   fixed_cost real not null,
   variable_cost real not null
)
--;; 11
CREATE INDEX geom_index ON candidates USING GIST(geometry);
--;; 12
CREATE TABLE networks (
   id serial not null primary key,
   map_id integer references maps(id) on delete cascade,
   name text,
   content text not null,
   created timestamp without time zone default (now() at time zone 'utc'),
   has_run boolean not null default false,
   job_id integer references jobs(id) on delete set null
);
--;; 13
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
        buildings.connection_cost as connection_cost,

        paths.start_id as start_id,
        paths.end_id as end_id,
        paths.length as length,
        paths.fixed_cost as fixed_cost,
        paths.variable_cost as variable_cost
FROM
        candidates
        LEFT JOIN buildings on candidates.id = buildings.id
        LEFT JOIN paths on candidates.id = paths.id
;
--;; 14
CREATE INDEX centroid_index ON candidates (ST_Centroid(geometry));
--;; 15
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
--;; 16
CREATE TABLE tilecache (
       x int not null,
       y int not null,
       z int not null,
       map_id integer not null references maps(id) on delete cascade,
       bytes bytea not null,
       primary key (x, y, z)
)
--;; 17
CREATE TABLE tilemaxima (
       z int not null,
       map_id int not null,
       maximum real not null,
       primary key (z, map_id)
);
--;; 18
CREATE TYPE user_auth AS ENUM ( 'normal', 'admin' )
--;; 19
CREATE TABLE users (
   id text not null primary key,   -- email address
   name text not null, -- display name
   auth user_auth not null, -- user type
   password text,
   reset_token text
)
--;; 20
CREATE TYPE project_auth AS ENUM ( 'read', 'write', 'admin' )
--;; 21
CREATE TABLE users_projects (
   project_id INTEGER REFERENCES projects(id) on delete cascade,
   user_id text references users(id) on delete cascade,
   auth project_auth not null, -- what authority user has over project
   primary key (project_id, user_id)
)
--;; 22
CREATE OR REPLACE VIEW ranked_jobs AS
SELECT id,state,queue_name,
       row_number()
        OVER (PARTITION BY queue_name,
              state in ('ready', 'running', 'cancel', 'cancelling')
              ORDER BY queued)
       as queue_position
FROM jobs;
--;; 23
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
--;; 24
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
--;; 25
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
--;;
ALTER TABLE candidates DROP CONSTRAINT candidates_map_id_fkey,
ADD CONSTRAINT candidates_map_id_fkey
  FOREIGN KEY (map_id) REFERENCES maps(id) ON DELETE CASCADE;

