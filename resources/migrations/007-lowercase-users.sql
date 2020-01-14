BEGIN TRANSACTION;
ALTER TABLE users_projects
  DROP CONSTRAINT users_projects_user_id_fkey,
  ADD CONSTRAINT users_projects_user_id_fkey
     FOREIGN KEY (user_id)
     REFERENCES users(id)
     ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE networks
  DROP CONSTRAINT networks_user_id_fkey,
  ADD CONSTRAINT networks_user_id_fkey
     FOREIGN KEY (user_id)
     REFERENCES users(id)
     ON DELETE CASCADE ON UPDATE CASCADE;
UPDATE users SET id = lower(id);
COMMIT;
--;;
