{
  network.name = "Hawk interactive deployment";

  thermos-ui = {config, pkgs, ...} : {
    imports = [ ./thermos.nix ];
    
    deployment.digitalOcean.size = "c-32";
    deployment.digitalOcean.region = "lon1";
    deployment.digitalOcean.image = "nixos20.09";
    deployment.sshKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCuI2T0KT2g5Z7xDfA36eypCTVUW+AZA6Q7/0Hvvdm3wqSyYHokw1Y7pLv/9+K/VL35vpfN368f5FAlbqYiM0p4UKrM7RgtgkyC77xg2ZXnJYNCwLHMph/GdteTgg/fBWXIzFeTMQCME3pILVhCYuQL2qJm0diihf7zuI1r+jxdwhMjY6BNNgfJ6SY1LZ7p0p9zGdGJGypEuGDFAhP5sGIiypLSpl/C0GOpYJzKUWR8MzMPsbfK/klpKS+AXGwmr27s8VmmzYFzueFExiFgDQ9N8EWO/XMtse1d4CLe+4tPYHzIqrIPdZXh11TdcmUL8g/lykOufPeLNmvXtEXCXxQx cse-server-root-key";

    deployment.keys.smtp.keyFile = ./smtp-password;

    services.thermos.ui.enable = true;
    services.thermos.model.enable = true;
    services.thermos.importer.enable = true;
    services.thermos.jre = pkgs.jre8;

    nixpkgs.config.allowUnfree = true;
    
    services.thermos.ui.baseUrl = "http://hawk.thermos-project.eu";
    services.thermos.ui.defaultUserAuth = ":unlimited";
    services.thermos.model.solverCount = 32;

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.fail2ban.enable = true;
    
    security.acme.acceptTerms = true;
    security.acme.email = "tom.hinton@cse.org.uk";
    
    services.nginx = {
      enable = true;
      clientMaxBodySize = "10g";
      virtualHosts."hawk.thermos-project.eu" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = { proxyPass = "http://localhost:${toString config.services.thermos.ui.port}/"; };
      };
    };
  };
}
