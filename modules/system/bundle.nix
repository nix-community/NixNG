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
in
{
  options.system.build = {
    bundle = mkOption {
      description = ''
        The full system bundle, including all dependencies.
      '';
      type = types.path;
    };
  };

  config.system.build.bundle = pkgs.runCommand (config.system.name + "-bundle") { } ''
    set -o pipefail
    mkdir -p $out
    xargs tar c < ${pkgs.writeReferencesToFile config.system.build.toplevel} | tar -xC $out/
  '';
}
