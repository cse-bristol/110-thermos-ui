{config, pkgs, lib, ...} :
with lib;
{
  options = {
    services.thermos = {
      jar = mkOption {
        type = types.path;
        default = ../target/thermos.jar;
      };
      jre = mkOption {
        type = types.package;
        default = pkgs.jre;
      };
      ui = {
        port = mkOption {
          type = types.int;
          default = 8080;
        };
        enable = mkOption {
          type = types.bool;
          default = true;
        };
        baseUrl = mkOption {
          type = types.str;
        };
        javaArgs = mkOption {
          type = types.str;
          default = "-Xmx8g -server";
        };
        defaultUserAuth = mkOption {
          type = types.str;
          default = ":basic";
        };
      };
      model = {
        enable = mkOption {
          type = types.bool;
          default = true;
        };
        scip = mkOption {
          type = types.package;
          default = (pkgs.callPackage ./scip.nix {});
        };
        solverCount = mkOption {
          type = types.int;
          default = 16;
        };
        javaArgs = mkOption {
          type = types.str;
          default = "-Xmx20g -server";
        };
      };
      importer = {
        enable = mkOption {
          type = types.bool;
          default = true;
        };

        importerCount = mkOption {
          type = types.int;
          default = 4;
        };

        javaArgs = mkOption {
          type = types.str;
          default = "-Xmx4g -server";
        };
      };
      postgresql = mkOption {
        type = types.package;
        default = pkgs.postgresql;
      };
    };
  };

  config =
    let
      cfg = config.services.thermos;
      pg = cfg.postgresql;

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

      environment.systemPackages = [
        pkgs.rxvt_unicode.terminfo

        (pkgs.stdenv.mkDerivation {
          name="get-lidar";
          src = ./get-lidar.sh;
          unpackPhase = ''
            true
          '';
          buildInputs = [pkgs.makeWrapper];
          installPhase = ''
            mkdir -p $out/bin
            makeWrapper $src $out/bin/get-lidar --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.gdal pkgs.jq pkgs.curl pkgs.findutils pkgs.unzip]}
          '';
        })
      ];



      systemd.services.thermos-web = mkIf cfg.ui.enable {
        serviceConfig = {
          TimeoutStopSec = "60s";
          Restart = "always";
        };

        wantedBy = ["multi-user.target"];

        script = ''
          export PG_HOST=10.233.1.2

          export SOLVER_COUNT=0
          export IMPORTER_COUNT=0
          export SMTP_ENABLED=true
          export WEB_SERVER_ENABLED=true
          export WEB_SERVER_PORT=${toString cfg.ui.port}

          export SMTP_HOST=smtp.hosts.co.uk
          export SMTP_PORT=25
          export SMTP_TLS=true
          export SMTP_USER=thermos-project.eu
          # export DEFAULT_USER_AUTH=${cfg.ui.defaultUserAuth}

          export LIDAR_DIRECTORY=/thermos-lidar/

          # while [[ ! -f /var/keys/smtp ]] ; do
          #   echo "waiting for smtp key"
          #   sleep 2
          # done

          # export SMTP_PASSWORD=$(cat /var/keys/smtp)
          # export SMTP_FROM_ADDRESS="THERMOS <system@thermos-project.eu>"
          # export WEB_SERVER_DISABLE_CACHE=false

          export BASE_URL="${cfg.ui.baseUrl}"


          exec ${cfg.jre}/bin/java "-XX:OnOutOfMemoryError=${oom-kill "email"} %p" ${cfg.ui.javaArgs} -jar ${cfg.jar}
        '';
      };

      systemd.services.thermos-model = mkIf cfg.model.enable {
        serviceConfig = {
          TimeoutStopSec = "60s";
          Restart = "always";
        };

        wantedBy = ["multi-user.target"];

        path = [cfg.model.scip];

        script = ''
          export PG_HOST=10.233.1.2
          export SOLVER_COUNT=${toString cfg.model.solverCount}
          export SMTP_ENABLED=false
          export WEB_SERVER_ENABLED=false
          export IMPORTER_COUNT=0
          export DEFAULT_USER_AUTH=${cfg.ui.defaultUserAuth}
          mkdir -p /root/bin
          export PATH="/root/bin:/run/current-system/sw/bin:$PATH"
          export GRB_LICENSE_FILE=/root/gurobi.lic
          echo '#!/bin/sh

          /run/current-system/sw/bin/gurobi_cl Method=3 Threads=3 LogFile="/root/gurobi-logs/gurobi-log-$(date +%Y-%m-%d-%H-%M-%S)" "$@"' > /root/bin/gurobi_cl
          chmod +x /root/bin/gurobi_cl
          exec ${cfg.jre}/bin/java "-XX:OnOutOfMemoryError=${oom-kill "problems"} %p" ${cfg.model.javaArgs} -jar ${cfg.jar}

        '';
      };

      systemd.services.thermos-importer = mkIf cfg.importer.enable {
        serviceConfig = {
          TimeoutStopSec = "60s";
          Restart = "always";
        };

        wantedBy = ["multi-user.target"];

        script = ''
          export PG_HOST=10.233.1.2
          export LIDAR_DIRECTORY=/thermos-lidar/

          export SOLVER_COUNT=0
          export SMTP_ENABLED=false
          export WEB_SERVER_ENABLED=false
          export IMPORTER_COUNT=${toString cfg.importer.importerCount}
          export DEFAULT_USER_AUTH=${cfg.ui.defaultUserAuth}
          exec ${cfg.jre}/bin/java "-XX:OnOutOfMemoryError=${oom-kill "imports"} %p" ${cfg.importer.javaArgs} -jar ${cfg.jar}
        '';
      };
    };
}
