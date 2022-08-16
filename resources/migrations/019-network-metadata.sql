ALTER TABLE networks ADD COLUMN meta json DEFAULT '{}'::json;
--;;
!thermos-backend.content-migrations.network-metadata/migrate
