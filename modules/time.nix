# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  lib,
  nglib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.time;
  inherit (lib) mkOption types;
in
{
  options.time = {
    tzdata = mkOption {
      description = ''
        `tzdata` package used for timezones.
      '';
      type = types.package;
      default = pkgs.tzdata;
    };

    timezone = mkOption {
      description = ''
        Which timezone will this NixNG system use.
      '';
      type = types.str;
      default = "Europe/Amsterdam";
    };
  };

  config = {
    environment.etc = {
      "zoneinfo" = {
        source = "${cfg.tzdata}/share/zoneinfo";
        mode = "symlink";
      };
      "localtime" = {
        source = "/etc/zoneinfo/${cfg.timezone}";
        mode = "symlink";
      };
    };
  };
}
