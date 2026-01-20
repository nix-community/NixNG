# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  pkgs,
  lib,
  config,
  nglib,
  ...
}:
let
  cfg = config.initrd;
in
{
  options.initrd = {
    enable = lib.mkEnableOption "Enable initrd as init";
  };

  config.init = lib.mkMerge [
    { availableInits = [ "initrd" ]; }
    (lib.mkIf cfg.enable {
      type = "initrd";
      script = nglib.writeSubstitutedShellScript {
        name = "init";
        file = ./init.sh;
        substitutes = with pkgs; {
          inherit eudev bash busybox;
        };
      };
    })
  ];
}
