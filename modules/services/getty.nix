# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.getty;

  optionsApplyFn = x:
    if isString x then
      x
    else if isList x then
      concatMapStringsSep " " optionsApplyFn x
    else
      toString x;
  optionsCreateArgument = k: v:
    if v != null then
      ''"--${k}" "${optionsApplyFn v}"''
    else
      ''"--${k}"'';
in
{
  options.services.getty = mkOption {
    description = "All of the agettys";
    type = types.attrsOf (types.submodule {
      options = {
        port = mkOption {
          description = "TTY port.";
          type = types.str;
        };

        baudrate = mkOption {
          description = "TTY baud rate.";
          type = with types;
            nullOr (oneOf [ (listOf int) int str ]);
          default = null;
        };

        term = mkOption {
          description = "TTY terminal name.";
          type = types.str;
          default = "vt100";
        };

        options = mkOption {
          description = "Extra options passed to the binary.";
          type = with types;
            let
              values = [ int str bool path (listOf (oneOf [ int str bool path ])) ];
            in
              attrsOf (nullOr (oneOf values));
          default = {};
        };

        package = mkOption {
          description = "getty package.";
          type = types.path;
          default = "${pkgs.util-linuxSystemdFree}/bin/agetty";
        };
      };
    });
    default = { };
  };
  config.init.services =  mapAttrs'
    (name: getty: nameValuePair "getty-${name}"
      {
        script = with getty;
          pkgs.writeShellScript "getty-${name}-run"
            ''
              exec ${package} \
                 "${port}" \
                 ${concatStringsSep " " (mapAttrsToList optionsCreateArgument options)}
                 "${optionalString
                   (baudrate != null)
                   (if isList baudrate then
                     concatMapStringsSep "," toString baudrate
                    else
                      toString baudrate)}" \
                 "${term}"
            '';
        enabled = true;
      })
    cfg;
}
