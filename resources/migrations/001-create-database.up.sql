CREATE TYPE candidate_type AS ENUM ('supply', 'demand', 'path');
--;;
CREATE TABLE candidates (
   id varchar(40) not null primary key,
   name varchar(512) not null,
   type candidate_type not null,
   subtype varchar(100) not null,
   geometry geometry(GEOMETRY, 4326) not null
);
--;;
CREATE TABLE buildings (
   id varchar(40) not null primary key
     references candidates(id) on delete cascade,
   connection_id text[] not null,
   demand real not null
);
--;;
CREATE TABLE paths (
   id varchar(40) not null primary key
     references candidates(id) on delete cascade,
   start_id varchar(40) not null,
   end_id varchar(40) not null
);
--;;
CREATE INDEX geom_index ON candidates USING GIST(geometry);
--;;
CREATE INDEX simple_geometry_index
ON candidates(ST_SimplifyPreserveTopology(geometry, 0.0001));
--;;
CREATE VIEW joined_candidates AS
SELECT candidates.id as id, name, "type", subtype,
array_to_string(connection_id, ',') as connection_id,
demand, start_id, end_id,
candidates.geometry as real_geometry,
ST_AsGeoJSON(geometry) as geometry,
ST_AsGeoJSON(ST_SimplifyPreserveTopology(geometry, 0.0001)) as simple_geometry
FROM candidates
LEFT JOIN buildings ON candidates.id = buildings.id
LEFT JOIN paths on candidates.id = paths.id;
