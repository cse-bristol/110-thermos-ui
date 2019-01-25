DROP TABLE jobs CASCADE
--;;
CREATE TYPE job_state AS ENUM (
  'ready', 'running', 'completed', 'cancel', 'cancelling', 'cancelled', 'failed'
)
--;;
CREATE TABLE jobs (
    id serial not null primary key,
    queue_name text not null,
    args text not null,
    state job_state not null default 'ready',
    queued timestamp without time zone default (now() at time zone 'utc'),
    updated timestamp without time zone default (now() at time zone 'utc')
);
--;;
CREATE OR REPLACE FUNCTION update_changetimestamp_column()
RETURNS TRIGGER AS $$
BEGIN
   NEW.updated = now(); 
   RETURN NEW;
END;
$$ language 'plpgsql';
--;;
CREATE TRIGGER update_jobs_changetimestamp BEFORE UPDATE
ON jobs FOR EACH ROW EXECUTE PROCEDURE 
update_changetimestamp_column();
