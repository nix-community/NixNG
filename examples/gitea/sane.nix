# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, nixpkgs, nixng }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-gitea-sane";
  config = ({ lib, config, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };

      imports = [
        (import "${nixng}/modules/services/gitea/sane.nix" {
          user = "gitea";
          database = {
            type = "sqlite3";
            path = "/var/lib/gitea/db.sqlite3";
          };
        })
      ];

      init.services.gitea.shutdownOnExit = true;
      services.gitea = {
        enable = true;

        secrets = {
          secretKey = {
            source.file = "/secret_key";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret SECRET_KEY > $_target
            '';
          };
          internalToken = {
            source.file = "/internal_token";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret INTERNAL_TOKEN > $_target
            '';
          };
          jwtSecret = {
            source.file = "/jwt_secret";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret JWT_SECRET > $_target
            '';
          };
          lfsJwtSecret = {
            source.file = "/lfs_jwt_secret";
            generate = ''
              _target="$1"
              ${lib.getExe config.services.gitea.package} generate secret LFS_JWT_SECRET > $_target
            '';
          };
        };

        settings = {};
      };
    }
  );
}
