CREATE TABLE projects (
   id serial not null primary key,
   name text not null,
   description text not null
)
--;;
CREATE TABLE maps (
   id serial not null primary key,
   project_id integer
      references projects(id)
      on delete cascade,
   name text not null
)
--;;
CREATE TYPE user_auth AS ENUM ( 'normal', 'admin' )
--;;
CREATE TABLE users (
   id text not null primary key,   -- email address
   name text not null, -- display name
   auth user_auth not null, -- user type
   password text not null
)
--;;
CREATE TYPE project_auth AS ENUM ( 'read', 'write', 'admin' )
--;;
CREATE TABLE users_projects (
   project_id INTEGER REFERENCES projects(id) on delete cascade,
   user_id text references users(id) on delete cascade,
   auth project_auth not null, -- what authority user has over project
   primary key (project_id, user_id)
)
--;;
ALTER TABLE candidates
ADD COLUMN map_id INTEGER
    REFERENCES maps (id)
    ON DELETE CASCADE;
--;;
ALTER TABLE problems
ADD COLUMN map_id INTEGER
    REFERENCES maps (id)
    ON DELETE CASCADE;
