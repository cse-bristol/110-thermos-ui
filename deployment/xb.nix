{
  network.name = "Cross-borough interactive deployment"; #probably in tag xboro

  xboro-interactive = {config, ...} : {
    imports = [ ./thermos.nix ];
    
    deployment.digitalOcean.size = "c-16";
    deployment.digitalOcean.region = "lon1";
    deployment.digitalOcean.image = "nixos20.09";
    deployment.sshKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCuI2T0KT2g5Z7xDfA36eypCTVUW+AZA6Q7/0Hvvdm3wqSyYHokw1Y7pLv/9+K/VL35vpfN368f5FAlbqYiM0p4UKrM7RgtgkyC77xg2ZXnJYNCwLHMph/GdteTgg/fBWXIzFeTMQCME3pILVhCYuQL2qJm0diihf7zuI1r+jxdwhMjY6BNNgfJ6SY1LZ7p0p9zGdGJGypEuGDFAhP5sGIiypLSpl/C0GOpYJzKUWR8MzMPsbfK/klpKS+AXGwmr27s8VmmzYFzueFExiFgDQ9N8EWO/XMtse1d4CLe+4tPYHzIqrIPdZXh11TdcmUL8g/lykOufPeLNmvXtEXCXxQx cse-server-root-key";

    deployment.keys.smtp.keyFile = ./smtp-password;

    services.thermos.ui.enable = true;
    services.thermos.model.enable = true;
    services.thermos.importer.enable = true;
    # machine total = 62Gi
    # reserve 20 for postgres & system
    # 42 left
    # 6 for web frontend => 36 left
    # 10 for importer, 26 for models
    services.thermos.ui.javaArgs = "-Xmx6g -server";
    services.thermos.model.javaArgs = "-Xmx20g -server -Xss1g";
    services.thermos.model.solverCount = 10;
    services.thermos.importer.javaArgs = "-Xmx6g -server";

    nixpkgs.config.allowUnfree = true;
    
    services.thermos.ui.baseUrl = "https://xb-thermos.re.cse.org.uk";

    networking.firewall.allowedTCPPorts = [ 80 443 ];
    networking.firewall.logRefusedConnections = false;
    networking.firewall.logRefusedPackets = false;
    networking.firewall.logReversePathDrops = false;
    services.thermos.ui.defaultUserAuth = ":unlimited";

    services.fail2ban.enable = true;
    
    security.acme.acceptTerms = true;
    security.acme.email = "tom.hinton@cse.org.uk";
    
    services.nginx = {
      enable = true;
      clientMaxBodySize = "10g";
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      virtualHosts."xb-thermos.re.cse.org.uk" = {
        appendConfig = ''
          gzip_types *;
          gzip_proxied any;
        '';

        forceSSL = true;
        enableACME = true;
        locations."/" = { proxyPass = "http://localhost:${toString config.services.thermos.ui.port}/"; };
      };
    };
  };
}
