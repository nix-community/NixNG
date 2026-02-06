# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-hydra";
  config = (
    { pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };
        nix = {
          loadNixDb = true;
          persistNix = "/nix-persist";
          config = {
            experimental-features = [
              "nix-command"
              "flakes"
            ];
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
        services.postgresql.package = pkgs.postgresql_17;
        services.socklog = {
          enable = true;
          unix = "/dev/log";
        };
      };
    }
  );
}
