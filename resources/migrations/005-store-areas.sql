ALTER TABLE buildings
   ADD COLUMN floor_area real default 0,
   ADD COLUMN height real default 0,
   ADD COLUMN wall_area real default 0,
   ADD COLUMN roof_area real default 0,
   ADD COLUMN ground_area real default 0;
--;; guess start values for areas
UPDATE buildings SET
   wall_area = ST_Perimeter(candidates.geometry :: geography) * 3,
   ground_area = ST_Area(candidates.geometry :: geography)
   FROM candidates WHERE candidates.id = buildings.candidate_id
;
--;;
UPDATE buildings SET roof_area = ground_area, floor_area = ground_area, height = 3;
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
        
        buildings.floor_area as floor_area,
        buildings.wall_area as wall_area,
        buildings.height as height,
        buildings.roof_area as roof_area,
        buildings.ground_area as ground_area,

        paths.start_id as start_id,
        paths.end_id as end_id,
        paths.length as length
FROM
        candidates
        LEFT JOIN buildings on candidates.id = buildings.candidate_id
        LEFT JOIN paths on candidates.id = paths.candidate_id
;
