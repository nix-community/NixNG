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
  cfg = config.iana;
in
{
  options.iana = {
    enable = lib.mkOption {
      description = "Enable /etc/services creation.";
      type = lib.types.bool;
      default = true;
    };

    package = lib.mkOption {
      description = "iana package.";
      type = lib.types.package;
      default = pkgs.iana-etc;
    };
  };

  config = {
    environment.etc."services" = {
      mode = "symlink";
      source = "${cfg.package}/etc/services";
    };
    environment.etc."protocols" = {
      mode = "symlink";
      source = "${cfg.package}/etc/protocols";
    };
  };
}
