nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-hydra";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };
        nix = {
          loadNixDb = true;
          overlayNix = "/nix-persist";
          config = {
            experimental-features = [ "nix-command" "flakes" ];
            sandbox = true;
            trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
            substituters = [ "https://cache.nixos.org/" ];
          };
        };
        services.hydra = {
          enable = true;
          hydraURL = "http://localhost:3000/";
          notificationSender = "root@example.org";
          useSubstitutes = true;
        };
        services.postgresql.package = pkgs.postgresql_12;
        services.socklog = {
          enable = true;
          unix = "/dev/log";
        };
      };
    }
  );
}
