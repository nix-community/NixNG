# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  pkgs,
  lib,
  config,
  ...
}:
let
  inherit (lib) mkOption types;

  cfg = config.system;
in
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
        ociConfig = {
          name = cfg.name;
          tag = "latest";
          maxLayers = 125;

          config = {
            StopSignal = "SIGCONT";
            Entrypoint = [ "${config.system.build.toplevel}/init" ];
          };
        };
      in
      with pkgs;
      {
        build = dockerTools.buildLayeredImage ociConfig;
        stream = dockerTools.streamLayeredImage ociConfig;
      };
  };
}
