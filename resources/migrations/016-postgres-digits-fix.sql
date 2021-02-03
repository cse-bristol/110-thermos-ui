DROP VIEW joined_candidates;
--;;
CREATE OR REPLACE VIEW joined_candidates
AS SELECT
        candidates.geoid as id,
        candidates.map_id as map_id,
        user_fields :: json as user_fields,
        candidates.geometry as raw_geometry,
        ST_AsGeoJson(candidates.geometry, 15) as geometry,

        buildings.candidate_id is not null as is_building,
        
        array_to_string(buildings.connection_id, ',') as connection_ids,
        buildings.demand_kwh_per_year as demand_kwh_per_year,
        buildings.demand_kwp as demand_kwp,
        buildings.connection_count as connection_count,
        coalesce(buildings.cooling_kwh_per_year,0) as cooling_kwh_per_year,
        coalesce(buildings.cooling_kwp,0) as cooling_kwp,
        
        buildings.floor_area as floor_area,
        buildings.wall_area as wall_area,
        buildings.height as height,
        buildings.roof_area as roof_area,
        buildings.ground_area as ground_area,
        buildings.demand_source as demand_source,
        buildings.conn_group as conn_group,

        paths.start_id as start_id,
        paths.end_id as end_id,
        paths.length as length
FROM
        candidates
        LEFT JOIN buildings on candidates.id = buildings.candidate_id
        LEFT JOIN paths on candidates.id = paths.candidate_id
;
