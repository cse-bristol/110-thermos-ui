{
  thermos-app = {pkgs, config, ...}:
  { deployment.targetEnv = "container";
    deployment.keys.smtp.keyFile = ./smtp-password;
    };
}
