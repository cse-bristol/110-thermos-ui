ALTER TYPE user_auth ADD VALUE 'restricted';
--;;
CREATE TYPE project_type AS ENUM (
    'restricted',
    'normal'
);
--;;
ALTER TABLE projects ADD COLUMN project_type project_type NOT NULL DEFAULT project_type('normal');
