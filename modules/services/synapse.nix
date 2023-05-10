# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, nglib, ... }:
let
  cfg = config.services.synapse;

  dataDir = "/var/syncthing";

  inherit (lib)
    mkOption
    types
    flip
    mapAttrs
    mkDefault
    mapAttrs'
    nameValuePair
    singleton
    mapAttrsToList
    isList
    concatMapStringsSep
    concatStringsSep
  ;

  configFormat = pkgs.formats.yaml {};


  workerSubmodule =
    { config, name, ... }:
    {
      options = {
        package = mkOption {
          type = types.package;
          default = pkgs.matrix-synapse;
          description = ''
            Package used for this worker.
          '';
        };

        settings = mkOption {
          type = configFormat.type;
          default = {};
          description = ''
            Configuration for this worker.
          '';
        };

        arguments = mkOption {
          type = with types;
            let
              values = [ str package int ];
            in
              attrsOf (oneOf ([ (listOf (oneOf values)) ] ++ values));
          default = {};
          description = ''
            Arguments for this worker.
          '';
        };
      };

      config = {
        settings = mapAttrs (_: mkDefault) {
          worker_replication_host = "127.0.0.1";
          worker_replication_http_port = 9093;
          worker_name = "worker-" + name;

          worker_log_config = "/dev/stdout";
        };

        arguments = {
          "config-path" = singleton (configFormat.generate "worker-${name}.yaml" config.settings);
        };
      };
    };
in
{
  options = {
    services.synapse.workers = mkOption {
      type = types.attrsOf (types.submodule workerSubmodule);
      default = {};
      description = ''
        Synapse worker instances.
      '';
    };
  };

  config = {
    init.services = flip mapAttrs' cfg.workers
      (n: v:
        nameValuePair
          "synapse-worker-${n}"
          {
            enabled = true;
            shutdownOnExit = true;
            script =
              let
              in
                pkgs.writeShellScript "synapse-worker-${n}.sh"
                  ''
                    ${pkgs.matrix-synapse}/bin/synapse_worker \
                      ${concatStringsSep "\n" (mapAttrsToList (n: v: if isList v then concatMapStringsSep "\n" (x: "--${n} \"${x}\"") v else "\"--${n} ${v}\"") v.arguments)}
                 '';
          }
      );
  };
}
