# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.services.getty;

  optionsApplyFn =
    x:
    if lib.isString x then
      x
    else if lib.isList x then
      lib.concatMapStringsSep " " optionsApplyFn x
    else
      toString x;
  optionsCreateArgument =
    k: v: if v != null then ''"--${k}" "${optionsApplyFn v}"'' else ''"--${k}"'';
in
{
  options.services.getty = lib.mkOption {
    description = "All of the agettys";
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          port = lib.mkOption {
            description = "TTY port.";
            type = lib.types.str;
          };

          baudrate = lib.mkOption {
            description = "TTY baud rate.";
            type =
              with lib.types;
              nullOr (oneOf [
                (listOf int)
                int
                str
              ]);
            default = null;
          };

          term = lib.mkOption {
            description = "TTY terminal name.";
            type = lib.types.str;
            default = "vt220";
          };

          options = lib.mkOption {
            description = "Extra options passed to the binary.";
            type =
              with lib.types;
              let
                values = [
                  int
                  str
                  bool
                  path
                  (listOf (oneOf [
                    int
                    str
                    bool
                    path
                  ]))
                ];
              in
              attrsOf (nullOr (oneOf values));
            default = { };
          };

          package = lib.mkOption {
            description = "getty package.";
            type = lib.types.path;
            default = pkgs.util-linuxSystemdFree;
          };
        };
      }
    );
    default = { };
  };
  config.init.services = lib.mapAttrs' (
    name: getty:
    lib.nameValuePair "getty-${name}" {
      script =
        with getty;
        pkgs.writeShellScript "getty-${name}-run" ''
          exec ${package}/sbin/agetty \
             "${port}" \
             ${lib.concatStringsSep " " (lib.mapAttrsToList optionsCreateArgument options)}
             "${
               lib.optionalString (baudrate != null) (
                 if lib.isList baudrate then concatMapStringsSep "," toString baudrate else toString baudrate
               )
             }" \
             "${term}"
        '';
      enabled = true;
    }
  ) cfg;
}
