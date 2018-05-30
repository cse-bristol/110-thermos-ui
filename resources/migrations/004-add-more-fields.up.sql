ALTER TABLE buildings
ADD COLUMN area real not null default 0;
--;;
ALTER TABLE paths
ADD COLUMN cost real not null default 0,
ADD COLUMN length real not null default 0;
--;;
ALTER TABLE candidates
ADD COLUMN orig_id text not null default '';
