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
  name = "nixng-pantalaimon";
  config = ({ pkgs, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.pantalaimon = {
        shutdownOnExit = true;
      };

      services.pantalaimon = {
        enable = true;

        package = (pkgs.pantalaimon.override
          { enableDbusUi = false; }).overrideAttrs (old: {
          version = "0.10.2";
          src = pkgs.fetchFromGitHub {
            owner = "matrix-org";
            repo = "pantalaimon";
            rev = "0.10.2";
            sha256 = "sha256-sjaJomKMKSZqLlKWTG7Oa87dXa5SnGQlVnrdS707A1w=";
          };
          patches = [ ];
        });

        config = {
          Default =
            {
              LogLevel = "Debug";
              SSL = "True";
              Notifications = "Off";
            };

          Clockwork =
            {
              Homeserver = "https://matrix.org";
              ListenAddress = "0.0.0.0";
              ListenPort = 80;
              SSL = "False";
            };
        };
      };
    });
}
