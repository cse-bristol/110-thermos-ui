# nixops configuration for a postgresql server, deployed into virtualbox
# with some data already in it for dev purposes
# You can make one with
# nixops create -d pg-server pg-server.nix
# nixops deploy -d pg-server

{
  pg-server = {cfg, pkgs, ...}:
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

        touch /var/lib/populated-db
      '';
    };
  };
}
