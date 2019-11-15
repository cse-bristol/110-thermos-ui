ALTER TABLE users ADD COLUMN changelog_seen integer DEFAULT 0,
                  ADD COLUMN created timestamp without time zone
                             default (now() at time zone 'utc');
--;;
UPDATE users SET changelog_seen = 0;
