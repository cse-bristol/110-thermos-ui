CREATE INDEX centroid_index ON candidates (ST_Centroid(geometry));
--;;
CREATE VIEW heat_centroids AS
SELECT
        candidates.geometry as geometry,
        ST_X(ST_Centroid(candidates.geometry)) as x,
        ST_Y(ST_Centroid(candidates.geometry)) as y,
        buildings.demand_kwh_per_year as demand
FROM
        candidates
        INNER JOIN buildings ON candidates.id = buildings.id
;
