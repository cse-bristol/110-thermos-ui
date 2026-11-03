CREATE TYPE parameter_scope AS ENUM ('system', 'user', 'project')
--;;
CREATE TABLE parameter_sets (
   id serial primary key,
   name text not null,
   description text not null default '',
   scope parameter_scope not null,
   -- the owner of a user set
   user_id text references users(id) on delete cascade,
   -- the project of a project set
   project_id integer references projects(id) on delete cascade,
   created_by text references users(id) on delete set null,
   -- EDN, a partial network document (see thermos-specs.parameters)
   content text not null,
   -- the names of the categories in content, separated by spaces
   categories text not null default '',
   -- ::document/version of content, for content migrations
   version integer not null default 0,
   -- goes up whenever content changes, so networks can tell
   revision integer not null default 1,
   is_system_default boolean not null default false,
   created timestamp without time zone default (now() at time zone 'utc'),
   updated timestamp without time zone default (now() at time zone 'utc'),
   check ((scope = 'project') = (project_id is not null)),
   check ((scope = 'user') = (user_id is not null))
)
--;;
CREATE TABLE parameter_set_defaults (
   user_id text not null references users(id) on delete cascade,
   -- null for the user's default everywhere
   project_id integer references projects(id) on delete cascade,
   parameter_set_id integer not null references parameter_sets(id) on delete cascade
)
--;;
CREATE UNIQUE INDEX parameter_set_defaults_unique
   ON parameter_set_defaults (user_id, coalesce(project_id, 0))
--;;
ALTER TABLE projects ADD COLUMN default_parameter_set_id integer
   REFERENCES parameter_sets(id) ON DELETE SET NULL
