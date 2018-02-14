# nixops configuration for a postgresql server, deployed into virtualbox
# with some data already in it for dev purposes

{
  pg_server = {cfg, pkgs, ...}:
  let pg = pkgs.postgresql;
      pgis = pkgs.postgis.override { postgresql = pg; } ;
  in
  {
    deployment.targetEnv = "virtualbox";

    environment.systemPackages = [pkgs.rxvt_unicode.terminfo];

    networking.firewall.allowedTCPPorts = [22 5432];

    services.postgresql = {
      enable = true;
      package = pg;
      extraPlugins = [ pgis.v_2_3_1 ];
      enableTCPIP = true;
      # Terrible, allow anyone to login
      # as any user over TCPIP
      # https://www.postgresql.org/docs/devel/static/auth-pg-hba-conf.html
      authentication = ''
        local all all trust
        host all all all trust
      '';

      # in production we would not do this here
      initialScript = ./create-database.sql;
    };

    systemd.services.populate-db = {
      path = with pkgs; [
        gdal pg
      ];
      wantedBy = ["multi-user.target"];
      after = ["postgresql.service"];
      requires = ["postgresql.service"];
      script =
      ''
        [[ -f /var/lib/populated-db ]] && exit 0

        psql -U root -d thermos_geometries -a -f "${./enable-postgis.sql}"

        load () {
           echo "load $1"
           ogr2ogr -f PostgreSQL 'pg:dbname=thermos_geometries user=postgres' "$1" -nln "$2" -overwrite
        }

        load "${./demand.geojson}" "old_demand"
        load "${./connections.geojson}" "old_connections"

        # run more SQL to transformulate it
        psql -U root -d thermos_geometries -a -f "${./populate-tables.sql}"

        touch /var/lib/populated-db
      '';
    };
  };
}
