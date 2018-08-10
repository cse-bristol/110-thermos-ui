{
  thermos-app = {pkgs, ...} :
  {
    deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.enableIpv6 = true;
    deployment.digitalOcean.region = "lon1";
    deployment.digitalOcean.size = "s-24vcpu-128gb";
    # when nixops 1.6 is released this will not be required

    imports = [./systemd-digitalocean/module.nix];
  };

  resources.sshKeyPairs.ssh-key = {};
}
