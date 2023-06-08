# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, config, ... }:
with lib;
{
  options.system.build = {
    ociImage = mkOption {
      description = ''
          OCI compatible image.
        '';
      type = types.submodule {
        options = {
          build = mkOption {
            description = ''
                A path to a OCI image in a gziped tarball.
              '';
            type = types.path;
          };
          stream = mkOption {
            description = ''
                A script which builds an OCI image and outputs what it builds
                into stdout without saving to disk.
              '';
            type = types.path;
          };
        };
      };
    };
  };

  config = {
    system.build.ociImage =
      let
        config = {
          name = cfg.name;
          tag = "latest";
          maxLayers = 125;

          config = {
            StopSignal = "SIGCONT";
            Entrypoint =
              [
                "${configFinal.system.build.toplevel}/init"
              ];
          };
        };
      in
        with pkgs; {
          build = dockerTools.buildLayeredImage config;
          stream = dockerTools.streamLayeredImage config;
        };
  };
}
