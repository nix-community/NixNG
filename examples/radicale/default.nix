{ nglib, nixpkgs, ...}:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-radicale";

  config = {...}: {
    dinit.enable = true;
    init.services.radicale = {
      shutdownOnExit = true;

      ensureSomething.create."stateDir" = {
        type = "directory";
        mode = "755";
        owner = "radicale:radicale";
        persistent = true;
        dst = "/var/lib/radicale";
      };
    };

    services.radicale = {
      enable = true;

      settings = {
        server = {
          hosts = "0.0.0.0:5232, [::]:5232";
          ssl = false;
        };

        auth = {
          type = "none";
        };

        storage = {
          filesystem_folder = "/var/lib/radicale/collections";
        };
      };
    };
  };
}
