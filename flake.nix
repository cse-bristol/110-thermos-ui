{
  description = "THERMOS web application";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      jdk = pkgs.jdk21;
      postgresql = pkgs.postgresql_17.withPackages (p: [ p.postgis ]);

      # The database helpers are in dev/test-env.sh; these just name them.
      pg-command = name: body: pkgs.writeShellScriptBin name ''
        set -euo pipefail
        root="$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
        ${body}
      '';
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          (pkgs.clojure.override { inherit jdk; })
          jdk
          pkgs.lessc
          pkgs.nodejs
          pkgs.git
          pkgs.curl
          pkgs.scipopt-scip
          postgresql
          (pg-command "start-postgres" ''"$root/dev/test-env.sh" start'')
          (pg-command "stop-postgres" ''"$root/dev/test-env.sh" stop'')
          (pg-command "reset-postgres" ''
            "$root/dev/test-env.sh" destroy
            "$root/dev/test-env.sh" start
          '')
        ];

        # target/resources is on the dev classpath, and the JVM ignores
        # directories that don't exist when it starts
        shellHook = ''
          mkdir -p target/resources
        '';

        IN_THERMOS_TEST_ENV = "1";
        PG_HOST = "127.0.0.2";
        SCIP_LIBRARY = "${pkgs.scipopt-scip}/lib/libscip.so";
      };
    };
}
