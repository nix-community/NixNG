# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

# These arguments are provided by the overarching NixNG repository and are not user confugurable.
{ nglib
, overlay
}:
# These arguments are user configurable
{ nixpkgs
, name
, system
, config
}:
with nixpkgs.lib;
let
  defaultModules = [
    ../modules/runit
    ../modules/dumb-init
    ../modules/initrd
    ../modules/init.nix
    ../modules/system.nix
    ../modules/assertions.nix
    ../modules/bootloader
    ../modules/nix.nix

    ../modules/security/ca.nix

    ../modules/misc/iana.nix

    ../modules/environment.nix
    ../modules/users.nix
    ../modules/ids.nix

    ../modules/services/apache2-nixos.nix
    ../modules/services/nginx.nix
    ../modules/services/gitea.nix
    ../modules/services/getty.nix
    ../modules/services/socklog.nix
    ../modules/services/crond.nix
    ../modules/services/hydra.nix
    ../modules/services/postgresql.nix
    ../modules/services/mysql.nix
    ../modules/services/certbot.nix
    ../modules/services/postfix.nix
    ../modules/services/dovecot.nix
    ../modules/services/pantalaimon.nix
    ../modules/services/jmusicbot.nix
    ../modules/services/php-fpm.nix
    ../modules/services/minecraft.nix
    ({ ... }: {
      system.name = name;
    })
  ];

  evaledModules = evalModules
    {
      modules = defaultModules ++ [
        config
        ({ ... }:
          {
            _module.args = {
              pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
              inherit system nglib;
            };
          }
        )
      ];
    };

  failedAssertions = map (x: x.message) (filter (x: !x.assertion) evaledModules.config.assertions);
  configValid =
    if failedAssertions != [ ] then
      throw "\nFailed assertions:\n${concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
    else
      evaledModules.config;
in
evaledModules // { config = configValid; }
