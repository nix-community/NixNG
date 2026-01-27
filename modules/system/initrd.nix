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
  bundleWithInit = pkgs.runCommand (config.networking.hostName + "-bundle-with-init") { } ''
    set -o pipefail
    mkdir -p $out
    ln -s ${config.system.build.toplevel}/init $out/init
    xargs tar c < ${pkgs.writeReferencesToFile config.system.build.toplevel} | tar -xC $out
  '';

in
{
  options.system.build = {
    initrd = lib.mkOption {
      description = ''
        Full system bundle packaged as a bootable initrd in cpio format.
      '';
      type = lib.types.path;
    };
  };

  config.system.build.initrd =
    pkgs.runCommand (config.networking.hostName + "-initrd")
      {
        nativeBuildInputs = with pkgs; [
          findutils
          cpio
          gzip
        ];
      }
      ''
        ( cd ${bundleWithInit} ; find . | cpio -o -H newc --quiet ) > $out
      '';
}
