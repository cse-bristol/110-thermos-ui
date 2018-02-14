CREATE ROLE postgres LOGIN;
GRANT root TO postgres;
ALTER USER postgres WITH PASSWORD 'therm0s';

CREATE DATABASE thermos_geometries;
