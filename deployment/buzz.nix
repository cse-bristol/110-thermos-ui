# for deployment with extra-container
# e.g. extra-container create buzz.nix --start --restart-changed
{
  # the container is called thermos-1 for historic reasons.
  containers.thermos-1 = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "10.233.5.1";
    localAddress = "10.233.5.2";

    # make key materials exist in the right place
    bindMounts."/var/keys" = {
      hostPath = "/srv/containers/110-thermos-tool/keys";
      isReadOnly = true;
    };

    config = {config, pkgs, ...}: {
      imports = [./thermos-server.nix];

      services.thermos.model.scip = pkgs.scipopt-scip;

      # required because we deployed a long time ago
      system.stateVersion = "21.11";
      
      nixpkgs.config.allowUnfree = true;
      
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

    };
  };
}
