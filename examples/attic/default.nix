{ nglib, nixpkgs, ... }:
let
  credentialsFile = "/run/attic.token";
in
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-attic";

  config = { pkgs, lib, config, ... }:
    let
      cfg = config.services.attic;
    in
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };

      init.services.attic = {
        shutdownOnExit = true;

        ensureSomething.exec."credentials" = {
          dst = credentialsFile;
          executable = pkgs.writers.writeBash "gen_attic_credentials.bash" ''
            ${lib.getExe pkgs.openssl} rand 64 | base64 -w0 > ${credentialsFile}
            export ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64="$(<${credentialsFile})"
            echo Use the following token to try out the server:
            echo '$ attic login demo http://<server>:<port> <token>'
            ${cfg.package}/bin/atticadm --config ${cfg.configFile} make-token --sub demo --validity '1 day' --pull '*' --push '*' --create-cache '*'
          '';
        };
      };

      services.attic = {
        enable = true;
        inherit credentialsFile;

        settings = {
          listen = "[::]:8080";
          database.url = "sqlite:///server.db?mode=rwc";

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
