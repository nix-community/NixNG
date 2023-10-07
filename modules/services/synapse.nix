# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, ... }:
let
  cfg = config.services.synapse;

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
    makeSearchPathOutput
    listToAttrs
    mkEnableOption
    optional
  ;

  configFormat = pkgs.formats.yaml {};

  workerDefaults = { config, name, ... }: {
    package = mkDefault cfg.package;
    settings = mapAttrs (_: mkDefault) {
      worker_name = "worker-" + name;
      worker_log_config = "/dev/stdout";
    };

    arguments = {
      "config-path" = singleton (configFormat.generate "worker-${name}.yaml" config.settings);
    };
  };

  mainDefaults = { config, name, ... }: {
    package = mkDefault pkgs.matrix-synapse;
  };

  instanceSubmodule =
    defaults:
    { config, name ? "", ... }@args:
    {
      options = {
        package = mkOption {
          type = types.package;
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

      config = defaults args;
    };

  synapseService =
    name: cfg: bin:
    {
      enabled = true;
      shutdownOnExit = true;
      script = pkgs.writeShellScript "synapse-worker-${name}.sh"
        ''
          ${cfg.package}/bin/${bin} \
            ${concatStringsSep " \\\n  " (flip mapAttrsToList cfg.arguments (n: v:
              if isList v then
                concatMapStringsSep " \\\n  " (x: "--${n} \"${x}\"") v
              else
                "--${n} \"${v}\""))}
        '';
   };
in
{
  options = {
    services.synapse = mkOption {
      type = types.submodule {
        imports = [
          (instanceSubmodule mainDefaults)
          {
            options = {
              enable = mkEnableOption "Enable main synapse process";
              workers = mkOption {
                type = types.attrsOf (types.submodule (instanceSubmodule workerDefaults));
                default = {};
                description = ''
                  Synapse worker instances.
                '';
              };
            };
          }
        ];
      };
      default = {};
      description = ''
        Synapse main instance.
      '';
    };
  };

  config = {
    init.services = listToAttrs ((flip mapAttrsToList cfg.workers
      (name: value:
        nameValuePair
          "synapse-worker-${name}"
          (synapseService name value "synapse_worker")

      ))  ++ (optional cfg.enable
        (nameValuePair
          "synapse"
          (synapseService "main" cfg "synapse_homeserver"))
      )
    );
  };
}
