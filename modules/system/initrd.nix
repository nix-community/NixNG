# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, config, ... }:
with lib;
let
  inherit (pkgs)
    writeReferencesToFile
    runCommandNoCC;

  bundleWithInit =
    runCommandNoCC (config.system.name + "-bundle-with-init")
      { }
      ''
        set -o pipefail
        mkdir -p $out
        ln -s ${config.system.build.toplevel}/init $out/init
        xargs tar c < ${writeReferencesToFile config.system.build.toplevel} | tar -xC $out
      '';

in
{
  options.system.build = {
    initrd = mkOption {
      description = ''
        Full system bundle packaged as a bootable initrd in cpio format.
      '';
      type = types.path;
    };
  };

  config.system.build.initrd =
    runCommandNoCC (config.system.name + "-initrd")
      {
        nativeBuildInputs = with pkgs; [
          findutils
          cpio
          gzip
        ];
      }
      ''
        ( cd ${bundleWithInit} ; find . | cpio -o -H newc --quiet ) > $out
      ''
  ;
}
