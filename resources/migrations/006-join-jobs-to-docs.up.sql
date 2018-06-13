ALTER TABLE problems
ADD COLUMN job integer references jobs(id)
ON DELETE SET NULL;
