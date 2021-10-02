/*
  * NixNG
  * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>
  *
  *  This file is free software: you may copy, redistribute and/or modify it
  *  under the terms of the GNU General Public License as published by the
  *  Free Software Foundation, either version 3 of the License, or (at your
  *  option) any later version.
  *
  *  This file is distributed in the hope that it will be useful, but
  *  WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  *  General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.getty;
in
{
  options.services.getty = mkOption {
    description = "All of the agettys";
    type = types.attrsOf (types.submodule {
      options = {
        baudRate = mkOption {
          description = "TTY baud rate";
          type = types.int;
        };
        termName = mkOption {
          description = "TTY terminal name";
          type = types.str;
          default = "vt100";
        };
        assume8BitTty = mkOption {
          description = "Whether to assume the tty is 8-bit";
          type = types.bool;
          default = true;
        };
        # TODO Local line -L
        pkg = mkOption {
          description = "getty package";
          type = types.path;
          default = "${pkgs.utillinuxMinimal}/bin/agetty";
        };
      };
    });
    default = { };
  };
  config.init.services = mapAttrs'
    (name: getty: nameValuePair "getty-${name}"
      {
        script = with getty;
          pkgs.writeShellScript "getty-${name}-run"
            ''
              exec setsid ${pkg} ${optionalString assume8BitTty "-8"} "${name}" "${toString baudRate}" "${termName}"
            '';
        enabled = true;
      })
    cfg;
}
