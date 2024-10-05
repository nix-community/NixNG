# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ config, lib, pkgs, ... }:
let
  cfg = config.services.socklog;
in
{
  options.services.socklog = {
    enable = lib.mkEnableOption "Enable socklog.";

    package = lib.mkOption {
      description = "Socklog package.";
      type = lib.types.package;
      default = pkgs.socklog;
    };

    unix = lib.mkOption {
      description = "Make socklog listen on a unix domain socket. Input the path to the UDS socklog should use.";
      type = with lib.types; nullOr path;
      default = null;
    };
    inet = lib.mkOption {
      description = "Make socklog listen on UDP.";
      type = with lib.types; nullOr (submodule {
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
  config = lib.mkIf cfg.enable {
    init.services.socklog =
      let
        unixSocklog =
          lib.optionalString (cfg.unix != null) "socklog unix ${cfg.unix} &";
        inetSocklog =
          lib.optionalString (cfg.inet != null) "socklog inter ${cfg.inet.ip} ${toString cfg.inet.port} &";
      in
      {
        script = pkgs.writeShellScript "socklog-run" ''
          set -m

          export PATH=${cfg.package}/bin:$PATH

          trap 'kill %1; kill %2' SIGINT SIGTERM
          ${unixSocklog}
          ${inetSocklog}
          fg
        '';
        enabled = true;
      };

    environment.systemPackages = [ cfg.package ];

    assertions = [
      {
        assertion = !(cfg.unix == null && cfg.inet == null);
        message = "You must select at least one listening endpoint for socklog, either `unix` or `inet`";
      }
    ];
  };
}
