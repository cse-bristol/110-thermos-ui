ALTER TABLE buildings
   ADD COLUMN cooling_kwh_per_year real default 0;
--;;
UPDATE buildings SET cooling_kwh_per_year = 0;
--;;
DROP VIEW joined_candidates;
--;;
CREATE OR REPLACE VIEW joined_candidates
AS SELECT
        candidates.geoid as id,
        candidates.map_id as map_id,
        candidates.name as name,
        candidates.type as type,
        candidates.geometry as raw_geometry,
        ST_AsGeoJson(candidates.geometry) as geometry,

        buildings.candidate_id is not null as is_building,
        
        array_to_string(buildings.connection_id, ',') as connection_ids,
        buildings.demand_kwh_per_year as demand_kwh_per_year,
        buildings.demand_kwp as demand_kwp,
        buildings.connection_count as connection_count,
        buildings.cooling_kwh_per_year as cooling_kwh_per_year,
        
        buildings.floor_area as floor_area,
        buildings.wall_area as wall_area,
        buildings.height as height,
        buildings.roof_area as roof_area,
        buildings.ground_area as ground_area,
        buildings.demand_source as demand_source,

        paths.start_id as start_id,
        paths.end_id as end_id,
        paths.length as length
FROM
        candidates
        LEFT JOIN buildings on candidates.id = buildings.candidate_id
        LEFT JOIN paths on candidates.id = paths.candidate_id
;
--;;
CREATE OR REPLACE VIEW heat_centroids AS
SELECT
        candidates.geometry as geometry,
        ST_X(ST_Centroid(candidates.geometry)) as x,
        ST_Y(ST_Centroid(candidates.geometry)) as y,
        buildings.demand_kwh_per_year as demand,
        map_id as map_id,
        buildings.cooling_kwh_per_year as cold_demand
FROM
        candidates
        INNER JOIN buildings ON candidates.id = buildings.candidate_id
;
--;;
ALTER TABLE tilecache
   ADD COLUMN is_heat boolean DEFAULT true,
   DROP CONSTRAINT tilecache_pkey,
   ADD CONSTRAINT tilecache_pkey PRIMARY KEY (x, y, z, map_id, is_heat)
;
--;;
ALTER TABLE tilemaxima
   ADD COLUMN is_heat boolean DEFAULT true,
   DROP CONSTRAINT tilemaxima_pkey,
   ADD CONSTRAINT tilemaxima_pkey PRIMARY KEY (z, map_id, is_heat);
--;;
UPDATE tilecache SET is_heat = true;
--;;
UPDATE tilemaxima SET is_heat = true;
--;;
ALTER TABLE buildings
   ADD COLUMN cooling_kwp real default 0;
--;;
UPDATE buildings SET cooling_kwp = 0;
--;;
DROP VIEW joined_candidates;
--;;
CREATE OR REPLACE VIEW joined_candidates
AS SELECT
        candidates.geoid as id,
        candidates.map_id as map_id,
        candidates.name as name,
        candidates.type as type,
        candidates.geometry as raw_geometry,
        ST_AsGeoJson(candidates.geometry) as geometry,

        buildings.candidate_id is not null as is_building,
        
        array_to_string(buildings.connection_id, ',') as connection_ids,
        buildings.demand_kwh_per_year as demand_kwh_per_year,
        buildings.demand_kwp as demand_kwp,
        buildings.connection_count as connection_count,
        buildings.cooling_kwh_per_year as cooling_kwh_per_year,
        buildings.cooling_kwp as cooling_kwp,
        
        buildings.floor_area as floor_area,
        buildings.wall_area as wall_area,
        buildings.height as height,
        buildings.roof_area as roof_area,
        buildings.ground_area as ground_area,
        buildings.demand_source as demand_source,

        paths.start_id as start_id,
        paths.end_id as end_id,
        paths.length as length
FROM
        candidates
        LEFT JOIN buildings on candidates.id = buildings.candidate_id
        LEFT JOIN paths on candidates.id = paths.candidate_id
;
