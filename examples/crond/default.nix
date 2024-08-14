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
  name = "nixng-crond";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };
        services.crond = {
          enable = true;
          crontabs = {
            hydra = {
              environment = {
                PATH = "${pkgs.busybox}/bin";
              };
              jobs = [
                ''*/2 * * * * root echo "asd"''
              ];
            };
          };
        };
      };
    }
  );
}
