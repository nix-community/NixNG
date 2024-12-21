# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  pkgs,
  lib,
  config,
  nglib,
  ...
}:
let
  cfg = config.runit;
  cfgSystem = config.system;
  cfgInit = config.init;
in
{
  options.runit = {
    enable = lib.mkEnableOption "Enable runit";

    pkg = lib.mkOption {
      description = "runit package to use";
      type = lib.types.package;
      default = pkgs.runit;
    };

    runtimeServiceDirectory = lib.mkOption {
      description = "where runsvdir should create superwise and log directories for services";
      type = lib.types.path;
      default = "/service";
    };

    stages = lib.mkOption {
      description = "runit stages";
      default = { };
      type = lib.types.submodule {
        options = {
          stage-1 = lib.mkOption {
            type = lib.types.path;
            description = "runit stage 1";
            default = pkgs.writeSubstitutedShellScript {
              name = "runit-stage-1";
              file = ./stage-1.sh;
              substitutes = {
                activationScript = cfgSystem.activationScript;
              };
            };
          };
          stage-2 = lib.mkOption {
            type = lib.types.path;
            description = "runit stage 2";
            default = pkgs.writeSubstitutedShellScript {
              name = "runit-stage-2";
              file = ./stage-2.sh;
              substitutes = {
                inherit (pkgs) runit busybox utillinux;
                inherit (cfg) runtimeServiceDirectory;
              };
            };
          };
          stage-3 = lib.mkOption {
            type = lib.types.path;
            description = "runit stage 3";
            default = pkgs.writeSubstitutedShellScript {
              name = "runit-stage-3";
              file = ./stage-3.sh;
              substitutes = { };
            };
          };
        };
      };
    };
    serviceDirectory = lib.mkOption {
      description = "Generated service directory";
      type = lib.types.path;
      readOnly = true;
    };
  };

  config = {
    system.activation."runit" = nglib.dag.dagEntryAnywhere ''
      export PATH=${pkgs.busybox}/bin
      mkdir -p ${cfg.runtimeServiceDirectory}

      function linkFarm() {
          src="$1"
          dst="$2"

          find "$src" -mindepth 1 -type d -print0 | sed -e "s~$src~~" | xargs -0 -I {} mkdir "$dst/{}"
          find "$src" -mindepth 1 -type f -print0 | sed -e "s~$src~~" | xargs -0 -I {} ln -s "$src/{}" "$dst/{}"
          find "$src" -mindepth 1 -type l -print0 | sed -e "s~$src~~" | xargs -0 -I {} cp "$src/{}" "$dst/{}"
      }

      linkFarm ${cfg.serviceDirectory} ${cfg.runtimeServiceDirectory}
    '';

    runit = {
      serviceDirectory = pkgs.runCommandNoCC "service-dir" { } ''
        mkdir $out
        ${lib.concatStringsSep "\n" (
          lib.mapAttrsToList (
            n: s:
            let
              run = pkgs.callPackage ./run.nix { inherit nglib; } { inherit n s; };
              finish = pkgs.callPackage ./finish.nix { } { inherit n s cfgInit; };
              log = pkgs.callPackage ./log.nix { } { inherit n s; };
            in
            ''
              mkdir -p $out/${n}/log
              ln -s ${run} $out/${n}/run
              ln -s ${finish} $out/${n}/finish
              ln -s ${log} $out/${n}/log/run
              ${lib.optionalString (!s.enabled) "touch $out/${n}/down"}
            ''
          ) cfgInit.services
        )}
      '';
    };

    init = lib.mkMerge [
      { availableInits = [ "runit" ]; }
      (lib.mkIf cfg.enable {
        type = "runit";
        shutdown = pkgs.writeShellScript "runit-shutdown" ''
          runit-init 0
        '';
        script = pkgs.writeShellScript "init" ''
          export PATH=${pkgs.busybox}/bin:${cfg.pkg}/bin \
                 _system_config="@systemConfig@"

          mkdir -p /etc/runit

          ln -sf ${cfg.stages.stage-1} /etc/runit/1
          ln -sf ${cfg.stages.stage-2} /etc/runit/2
          ln -sf ${cfg.stages.stage-3} /etc/runit/3

          exec runit-init
        '';
      })
    ];
  };
}
