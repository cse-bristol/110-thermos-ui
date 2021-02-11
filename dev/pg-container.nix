{config, pkgs, ...}:
let
  pg = pkgs.postgresql;
  pgis = pkgs.postgis.override { postgresql = pg; };
  enable-postgis = builtins.toFile "enable-postgis.sql"
  "CREATE EXTENSION postgis; CREATE EXTENSION postgis_raster;";
in
{
  networking.firewall.allowedTCPPorts = [ 5432 ];

  services.postgresql = {
    enable = true;
    package = pg;
    extraPlugins = [ pgis ];
    enableTCPIP = true;

    authentication = ''
      local all all trust
      host all all all trust
    '';

    initialScript = builtins.toFile "create-database.sql"
    ''
      CREATE ROLE postgres LOGIN;
      GRANT root TO postgres;
      ALTER USER postgres WITH PASSWORD 'therm0s';
      CREATE DATABASE thermos;
    '';

    settings = {
      wal_level = "minimal";
      wal_compression = true;
      max_wal_senders = 0;
      archive_mode = false;
      max_wal_size = "8GB";
    };
  };

  systemd.services.postgresql.environment.POSTGIS_GDAL_ENABLED_DRIVERS="ENABLE_ALL";

  systemd.services.enable-postgis = {
    path = [pg];
    wantedBy = ["multi-user.target"];
    after = ["postgresql.service"];
    requires = ["postgresql.service"];
    script = ''
      [[ -f /var/lib/enabled-postgis ]] ||
      ( psql -U postgres -d thermos -a -f "${enable-postgis}" &&
      touch /var/lib/enabled-postgis )
    '';
  };

}
