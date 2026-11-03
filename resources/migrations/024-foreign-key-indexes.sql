CREATE INDEX IF NOT EXISTS networks_map_id_idx ON networks (map_id);
--;;
CREATE INDEX IF NOT EXISTS networks_job_id_idx ON networks (job_id);
--;;
CREATE INDEX IF NOT EXISTS maps_project_id_idx ON maps (project_id);
--;;
CREATE INDEX IF NOT EXISTS users_projects_user_id_idx ON users_projects (user_id);
