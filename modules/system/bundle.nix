# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

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

  config.system.build.bundle = pkgs.runCommand (config.networking.hostName + "-bundle") { } ''
    set -o pipefail
    mkdir -p $out
    xargs tar c < ${pkgs.writeReferencesToFile config.system.build.toplevel} | tar -xC $out/
  '';
}
