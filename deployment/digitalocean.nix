{
  thermos-app = {pkgs, ...} :
  {
    deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.enableIpv6 = true;
    deployment.digitalOcean.region = "lon1";
    deployment.digitalOcean.size = "s-6vcpu-16gb";
    deployment.keys.smtp.keyFile = ./smtp-password;
  };

  resources.sshKeyPairs.ssh-key = {};
}
