# Use channel 23.05 - see "StepByStepBuzzAppsToDigitalOcean.md"
# export NIX_PATH=nixpkgs=channel:nixos-23.05

{
  network = {
    description = "THERMOS application";
    name = "thermos-live";
    defaults = {
      deployment = {
        digitalOcean.region = "lon1";
        # s-8vcpu-16gb-480gb-intel - disk big enough to unzip db backup for patching
        # To get a list of slug sizes use - nix-shell -p doctl --command "doctl compute size list"
        digitalOcean.size = "s-8vcpu-16gb-480gb-intel";
        digitalOcean.image = "nixos24.11"; # "nixos20.09"; "nixos24.11";
        sshKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCuI2T0KT2g5Z7xDfA36eypCTVUW+AZA6Q7/0Hvvdm3wqSyYHokw1Y7pLv/9+K/VL35vpfN368f5FAlbqYiM0p4UKrM7RgtgkyC77xg2ZXnJYNCwLHMph/GdteTgg/fBWXIzFeTMQCME3pILVhCYuQL2qJm0diihf7zuI1r+jxdwhMjY6BNNgfJ6SY1LZ7p0p9zGdGJGypEuGDFAhP5sGIiypLSpl/C0GOpYJzKUWR8MzMPsbfK/klpKS+AXGwmr27s8VmmzYFzueFExiFgDQ9N8EWO/XMtse1d4CLe+4tPYHzIqrIPdZXh11TdcmUL8g/lykOufPeLNmvXtEXCXxQx cse-server-root-key"; 
      };
    };
  };

  thermos-app = import ./thermos-server-do.nix;
}
