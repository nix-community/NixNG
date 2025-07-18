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
  config = (
    { pkgs, lib, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.pantalaimon = {
        shutdownOnExit = true;
      };

      nixpkgs.config.allowInsecurePredicate = pkg: lib.getName pkg == "olm";

      services.pantalaimon = {
        enable = true;

        config = {
          Default = {
            LogLevel = "Debug";
            SSL = "True";
            Notifications = "Off";
          };

          Clockwork = {
            Homeserver = "https://matrix.org";
            ListenAddress = "0.0.0.0";
            ListenPort = 80;
            SSL = "False";
          };
        };
      };
    }
  );
}
