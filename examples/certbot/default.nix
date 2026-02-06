# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-certbot";
  config = (
    { config, ... }:
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
              extraDomains = [ "hydra.redalder.org" ];
              webroot = "/var/www/acme";
              email = "admin@redalder.org";
            };
          };
        };
      };
    }
  );
}
