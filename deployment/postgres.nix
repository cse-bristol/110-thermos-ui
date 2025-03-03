{config, pkgs, lib, ...} :

let
  pg = pkgs.postgresql;
  pgis = pkgs.postgis.override { postgresql = pg; };
  enable-postgis = builtins.toFile "enable-postgis.sql"
  "CREATE EXTENSION postgis; CREATE EXTENSION postgis_raster;";

  oom-kill = queue : (pkgs.writeShellScript "handle-oom.sh" ''
        ${pkgs.curl}/bin/curl -d "$2" -H "Title: THERMOS ${queue} OOM" https://ntfy.re.cse.org.uk/system-status
        kill -TERM $1
        sleep 3
        if kill -0 $1; then
           kill -9 $1
        fi
        /run/wrappers/bin/su postgres -c "${pg}/bin/psql -d thermos -c \"update jobs set state='failed', message=message || '\n----\nOut of memory!' where state='running' and queue_name='${queue}'\""
      '');
in {
  networking.firewall.allowedTCPPorts = [ 5432 ];
  environment.systemPackages = [
    pkgs.vim
    pkgs.ranger
    pkgs.cron
  ];
  
  services.cron = {
    enable = true;
  };

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
