# nixops configuration for a postgresql server, deployed into virtualbox
# with some data already in it for dev purposes
# You can make one with
# nixops create -d pg-server pg-server.nix
# nixops deploy -d pg-server

{
  network.description = "THERMOS dev database server";

  thermos-server = {pkgs, config, ...} :
  let
    pg = pkgs.postgresql;
  pgis = pkgs.postgis.override { postgresql = pg; };
  create-database = builtins.toFile "create-database.sql"
  ''
    CREATE ROLE postgres LOGIN;
    GRANT root TO postgres;
    ALTER USER postgres WITH PASSWORD 'therm0s';
    CREATE DATABASE thermos;
  '';
  enable-postgis = builtins.toFile "enable-postgis.sql"
    "CREATE EXTENSION postgis;";
  in
  {
    deployment.targetEnv = "virtualbox";

    environment.systemPackages = [pkgs.rxvt_unicode.terminfo];

    networking.firewall.allowedTCPPorts = [ 22 5432 ];

    services.postgresql = {
      enable = true;
      package = pg;
      extraPlugins = [ pgis ];
      enableTCPIP = true;

      authentication = ''
        local all all trust
        host all all all trust
      '';

      initialScript = create-database;
    };

    systemd.services.enable-postgis = {
      path = [pg];
      wantedBy = ["multi-user.target"];
      after = ["postgresql.service"];
      requires = ["postgresql.service"];
      script = ''
        [[ -f /var/lib/enabled-postgis ]] ||
        ( psql -U root -d thermos -a -f "${enable-postgis}" &&
          touch /var/lib/enabled-postgis )
      '';
    };
  };
}
