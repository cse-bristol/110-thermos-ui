{pkgs, config, ...} :
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
  "CREATE EXTENSION postgis; CREATE EXTENSION postgis_raster;";
  scip = (pkgs.callPackage ./scip.nix {});
in
{
  ####
  # Copied from "buzz.nix"
  # See https://nixops.readthedocs.io/en/latest/overview.html#managing-keys
  # These are copied to /run/keys
  deployment.keys.smtp.keyFile = ./smtp-password;
  
  ###
  # Below line needs to be commented out when first creating DO instance
  deployment.keys.smtp.transient = false;
  ###

  deployment.keys.spaces-access-key.keyFile = ./spaces-access-key;
  deployment.keys.spaces-secret-key.keyFile = ./spaces-secret-key;
  deployment.keys.backup-file-name.keyFile = ./backup-file-name-do;
  ####

  imports = [ ./thermos.nix ];
  
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  nixpkgs.config.allowUnfree = true;
  
  services.thermos.ui.enable = true;
  services.thermos.model.enable = true;
  services.thermos.importer.enable = true;

  ##
  # Copied from "hawk.nix" ...
  services.thermos.ui.defaultUserAuth = ":unlimited";
  services.thermos.model.solverCount = 32;
  ##

  services.thermos.jre = pkgs.jdk11;		# Same version as in use by clojure when building jar. There is no "jre11" in channel 23.05.

  #services.thermos.ui.baseUrl = "https://tool.thermos-project.eu";
  services.thermos.ui.baseUrl = "https://dr.thermos.cse.org.uk";
  
  security.acme = {
    email = "robots@cse.org.uk";
    acceptTerms = true;
  };

  services.nginx = {
    enable = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;

    #virtualHosts."xx.xx.xx.xx" = {
    virtualHosts."dr.thermos.cse.org.uk" = {
    #virtualHosts."thermos-project.eu" = {
      forceSSL = true;
      enableACME = true;

      extraConfig = ''
        client_max_body_size 1000M;
        gzip on;
        gzip_proxied any;
        gzip_types text/css text/javascript application/json text/plain text/xml application/javascript application/octet-stream;
        error_page 403 404 500 502 503 504 /error.html;
      '';

      locations = {
        "/" = {
          proxyPass = "http://localhost:${toString config.services.thermos.ui.port}/";
          extraConfig = ''
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_read_timeout 7200;
            proxy_send_timeout 7200;
            send_timeout 7200;
          '';
        };
        "/error.html" = {
          extraConfig = ''
            internal;
          '';
          root = "${./error}";
        };
      };
    };

    appendHttpConfig = ''
      # GZIP more MIME types than in the recommended gzip settings
      gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript application/x-javascript image/svg+xml text/html application/xhtml+xml;

      add_header Strict-Transport-Security max-age=15768000;
    '';
  };

  security.sudo.wheelNeedsPassword = false;

  ###
  # From "buzz.nix"
  # Backup postgres to digitalocean S3
  services.postgresqlBackup.enable = true;

  systemd.services.uploadBackup = {
      path = [ pkgs.s3cmd ];
      script = ''
        s3cmd --access_key=$(cat /var/keys/spaces-access-key) \
              --secret_key=$(cat /var/keys/spaces-secret-key) \
              --host ams3.digitaloceanspaces.com \
              --host-bucket='%(bucket)s.ams3.digitaloceanspaces.com' \
              --force \
           put ${config.services.postgresqlBackup.location}/all.sql.gz \
           s3://thermos-backup/$(cat /var/keys/backup-file-name)-$(date +%a)
      '';
      
      # yuck
      after = [
        "postgresqlBackup.service"
      ];
      wantedBy = [
        "postgresqlBackup.service"
      ];
  };
  ###
  
  users.motd = ''
    THERMOS demo server
    -------------------
    LIDAR is stored in /thermos-lidar/
  '';
}
