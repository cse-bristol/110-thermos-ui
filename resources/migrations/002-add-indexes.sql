CREATE INDEX buildings_gix ON buildings USING GIST (geometry);
--;;
CREATE INDEX ways_gix ON ways USING GIST (geometry);
