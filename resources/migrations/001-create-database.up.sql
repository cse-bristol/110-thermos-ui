CREATE TABLE candidates (
   id varchar(40) not null primary key,
   orig_id text not null,
   name text not null,
   type text not null,
   geometry geometry(GEOMETRY, 4326) not null
)
--;;
CREATE TABLE buildings (
   id text not null primary key
     references candidates(id) on delete cascade,
   connection_id text[] not null,
   demand_kwh_per_year real not null,
   demand_kwp real not null,
   connection_count int not null
)
--;;
CREATE TABLE paths (
   id text not null primary key
     references candidates(id) on delete cascade,
   start_id text not null,
   end_id text not null,
   length real not null,
   unit_cost real not null
)
--;;
CREATE INDEX geom_index ON candidates USING GIST(geometry);
--;;
CREATE TABLE jobs (
    id serial not null primary key,
    args text not null,
    state smallint not null,
    queue text not null
);
--;;
CREATE TABLE problems (
    id serial not null primary key,
    org varchar(512) not null,
    name varchar(512) not null,
    content text not null,
    created timestamp without time zone default (now() at time zone 'utc'),
    format int not null default 0,
    has_run boolean not null default false,
    job integer references jobs(id) on delete set null
);
--;;
CREATE INDEX job_state_idx on jobs (state, id);
--;;
CREATE INDEX job_state_queue_idx on jobs (state,queue,id);
--;;
CREATE VIEW joined_candidates AS
SELECT
        candidates.id as id,
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
