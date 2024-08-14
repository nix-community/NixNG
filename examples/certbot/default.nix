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
  name = "nixng-certbot";
  config = ({ config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };

        services.certbot = {
          enable = true;

          acceptTerms = true;

          domains = {
            "redalder.org" = {
              extraDomains = [
                "hydra.redalder.org"
              ];
              webroot = "/var/www/acme";
              email = "admin@redalder.org";
            };
          };
        };
      };
    }
  );
}
