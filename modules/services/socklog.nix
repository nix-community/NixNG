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

{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.socklog;
in
{
  options.services.socklog = {
    enable = mkEnableOption "Enable socklog.";

    unix = mkOption {
      description = "Make socklog listen on a unix domain socket. Input the path to the UDS socklog should use.";
      type = with types; nullOr path;
      default = null;
    };
    inet = mkOption {
      description = "Make socklog listen on UDP.";
      type = with types; nullOr (submodule {
        options = {
          ip = mkOption {
            description = ''
              The IP address on which to listen on, must be an interface
              or 0 for all. Doesn't accept `localhost`.
            '';
            type = types.str;
            default = "127.0.0.1";
          };
          port = mkOption {
            description = "The port on which to listen on.";
            type = types.port;
            default = 514;
          };
        };
      });
      default = null;
    };
  };
  config = mkIf cfg.enable {
    init.services.socklog =
      let
        unixSocklog =
          optionalString (cfg.unix != null) "socklog unix ${cfg.unix} &";
        inetSocklog =
          optionalString (cfg.inet != null) "socklog inter ${cfg.inet.ip} ${toString cfg.inet.port} &";
      in
      {
        script = pkgs.writeShellScript "socklog-run" ''
          set -m

          export PATH=${pkgs.socklog}/bin:$PATH

          trap 'kill %1; kill %2' SIGINT SIGTERM
          ${unixSocklog}
          ${inetSocklog}
          fg
        '';
        enabled = true;
      };

    assertions = [
      {
        assertion = !(cfg.unix == null && cfg.inet == null);
        message = "You must select at least one listening endpoint for socklog, either `unix` or `inet`";
      }
    ];
  };
}
