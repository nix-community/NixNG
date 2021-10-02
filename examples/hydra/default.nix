/*
  * NixNG
  * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>
  *
  *  This file is free software: you may copy, redistribute and/or modify it
  *  under the terms of the GNU General Public License as published by the
  *  Free Software Foundation, either version 3 of the License, or (at your
  *  option) any later version.
  *
  *  This file is distributed in the hope that it will be useful, but
  *  WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  *  General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

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
