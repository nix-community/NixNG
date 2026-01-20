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
  bundleWithInit = pkgs.runCommand (config.system.name + "-bundle-with-init") { } ''
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
    pkgs.runCommand (config.system.name + "-initrd")
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
