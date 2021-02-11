{
  network.name = "CDDP interactive deployment"; #probably in tag cddp

  cddp-interactive = {config, ...} : {
    imports = [ ./thermos.nix ];
    
    deployment.digitalOcean.size = "c-32";
    deployment.digitalOcean.region = "lon1";
    deployment.digitalOcean.image = "nixos20.09";
    deployment.sshKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCuI2T0KT2g5Z7xDfA36eypCTVUW+AZA6Q7/0Hvvdm3wqSyYHokw1Y7pLv/9+K/VL35vpfN368f5FAlbqYiM0p4UKrM7RgtgkyC77xg2ZXnJYNCwLHMph/GdteTgg/fBWXIzFeTMQCME3pILVhCYuQL2qJm0diihf7zuI1r+jxdwhMjY6BNNgfJ6SY1LZ7p0p9zGdGJGypEuGDFAhP5sGIiypLSpl/C0GOpYJzKUWR8MzMPsbfK/klpKS+AXGwmr27s8VmmzYFzueFExiFgDQ9N8EWO/XMtse1d4CLe+4tPYHzIqrIPdZXh11TdcmUL8g/lykOufPeLNmvXtEXCXxQx cse-server-root-key";

    deployment.keys.smtp.keyFile = ./smtp-password;

    services.thermos.ui.enable = true;
    services.thermos.model.enable = true;
    services.thermos.importer.enable = true;

    services.thermos.ui.javaArgs = "-Xmx6g -server";
    services.thermos.model.javaArgs = "-Xmx32g -server";
    services.thermos.importer.javaArgs = "-Xmx10g -server";

    nixpkgs.config.allowUnfree = true;
    
    services.thermos.ui.baseUrl = "https://cddp-thermos.cse.org.uk";

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    security.acme.acceptTerms = true;
    security.acme.email = "tom.hinton@cse.org.uk";
    
    services.nginx = {
      enable = true;
      clientMaxBodySize = "10g";
      virtualHosts."cddp-thermos.cse.org.uk" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = { proxyPass = "http://localhost:${toString config.services.thermos.ui.port}/"; };
      };
    };
  };
}
