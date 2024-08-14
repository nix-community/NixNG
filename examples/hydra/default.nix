# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
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
          persistNix = "/nix-persist";
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
