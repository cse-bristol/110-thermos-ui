ALTER TABLE users ALTER COLUMN auth TYPE VARCHAR(255);
--;;
UPDATE users SET auth = 'unlimited' WHERE auth = 'normal';
--;;
DROP TYPE IF EXISTS user_auth;
--;;
CREATE TYPE user_auth AS ENUM ('basic', 'intermediate', 'unlimited', 'admin');
--;;
ALTER TABLE users ALTER COLUMN auth TYPE user_auth USING (auth::user_auth);
