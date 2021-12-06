# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, nixpkgs }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-minecraft";
  config = ({ pkgs, config, ... }:
    with pkgs.lib;
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };
        init.services.minecraft = {
          shutdownOnExit = true;
        };

        services.minecraft = {
          enable = true;
          eulaAccept = true;

          forgeZipFile = builtins.fetchurl {
            url = "https://media.forgecdn.net/files/3551/162/SIMPLE-SERVER-FILES-1.8.12.zip";
            sha256 = "sha256:16w4874vbc8zab6czixmx62i5hxfv1zkjcbfz9djmhwwa8inw02l";
          };
          forgeFetchedHash = "sha256-58HAjgrbtVb62vJKdfzXTIJRSycP1cDnp4h5/mnIwtY=";

          forgeConfigOverrides = {};
        };
      };
    });
}
