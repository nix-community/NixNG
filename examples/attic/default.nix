{ nglib, nixpkgs, ... }:
let
  credentialsFile = "/run/attic.token";
in
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-attic";

  config = { pkgs, lib, ... }: {
    dumb-init = {
      enable = true;
      type.services = { };
    };

    init.services.attic = {
      shutdownOnExit = true;

      ensureSomething.exec."credentialsFile" = {
        dst = credentialsFile;
        executable = pkgs.writers.writeBash "gen_attic_token.bash" ''
          ${lib.getExe pkgs.openssl} rand 64 | base64 -w0 > ${credentialsFile}
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
