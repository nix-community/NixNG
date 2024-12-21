{ nglib, nixpkgs, ... }:
let
  credentialsFile = "/tmp/dummy";
in
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-attic";

  config =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      cfg = config.services.attic;
    in
    {
      dinit.enable = true;

      init.services.attic = {
        shutdownOnExit = true;

        ensureSomething.exec."credentials" = {
          dst = credentialsFile;

          executable = pkgs.writers.writeBash "gen_attic_credentials.bash" ''
            echo Use the following token to try out the server:
            echo '$ attic login demo http://<server>:<port> <token>'
            ${cfg.package}/bin/atticadm --config ${cfg.configFile} make-token --sub demo --validity '1 day' --pull '*' --push '*' --create-cache '*'
            touch ${credentialsFile}
          '';
        };
      };

      services.attic = {
        enable = true;

        settings = {
          listen = "[::]:8080";
          database.url = "sqlite:///server.db?mode=rwc";
          token-hs256-secret-base64 = "kONlkVtBeH1PPoc7jLo0X3xKnNzuLhwYf030ghOTCH817P6jzqotxuhzRSrlOxS/VAmb5UEDobgw21EFGk8+XA==";

          storage = {
            type = "local";
            path = "/var/lib/atticd/storage";
          };

          chunking = {
            nar-size-threshold = 64 * 1024;
            min-size = 16 * 1024;
            avg-size = 64 * 1024;
            max-size = 256 * 1024;
          };
        };
      };
    };
}
