{
  thermos-app = {pkgs, config, ...}:
  {
    deployment.targetEnv = "container";
    deployment.keys.smtp.keyFile = ./smtp-password;
    deployment.keys.spaces-access-key.keyFile = ./spaces-access-key;
    deployment.keys.spaces-secret-key.keyFile = ./spaces-secret-key;
    deployment.keys.backup-file-name.keyFile = ./backup-file-name;

    nixpkgs.config.allowUnfree = true;
    
    # Backup postgres to digitalocean S3
    services.postgresqlBackup.enable = true;

    systemd.services.uploadBackup = {
      path = [ pkgs.s3cmd ];
      script = ''
        s3cmd --access_key=$(cat /run/keys/spaces-access-key) \
              --secret_key=$(cat /run/keys/spaces-secret-key) \
              --host ams3.digitaloceanspaces.com \
              --host-bucket='%(bucket)s.ams3.digitaloceanspaces.com' \
              --force \
           put ${config.services.postgresqlBackup.location}/all.sql.gz \
           s3://thermos-backup/$(cat /run/keys/backup-file-name)-$(date +%a)
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
}
