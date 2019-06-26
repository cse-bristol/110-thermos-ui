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

        paths.start_id as start_id,
        paths.end_id as end_id,
        paths.length as length,
        paths.fixed_cost as fixed_cost,
        paths.variable_cost as variable_cost
FROM
        candidates
        LEFT JOIN buildings on candidates.id = buildings.candidate_id
        LEFT JOIN paths on candidates.id = paths.candidate_id
;
--;;
ALTER TABLE buildings DROP COLUMN connection_cost;
