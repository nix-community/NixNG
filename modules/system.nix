# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, nglib, config, ... }:
with lib;
let
  cfg = config.system;

  configFinal = config;
in
{
  options.system = {
    createNixRegistration = mkEnableOption
      ''
        Create $out/registration, which allows one to create and populate
        the Nix database at start up, useful when building container images,
        which must be able to use Nix themselves.
      '';

    build = mkOption {
      default = {};
      description = lib.mdDoc ''
        Attribute set of derivations used to set up the system.
      '';
      type = types.submoduleWith {
        modules = [{
          freeformType = with types; lazyAttrsOf (uniq unspecified);
        }];
      };
    };

    activation = mkOption {
      description = ''
        A set of shell script fragments that are executed when a NixNG system
        configuration is activated. You can update /etc,
        create accounts, and so on. For creating service related directories or file,
        please use <option>init.services.<service>.ensureSomething</option>.
        These fragments are isolated and can't effect each other's environment.
        These are run every time the system configuration is activated, which also
        happens at boot, therefore it's important that these scripts are idempotent
        and fast.
      '';
      type = with types; attrsOf (submodule {
        options = {
          data = mkOption {
            description = ''
              Script fragment which to run.
            '';
            type = types.str;
          };
          before = mkOption {
            description = ''
              Script before dependencies. See <literal>/lib/dag.nix</literal>.
            '';
            type = with types; listOf str;
          };
          after = mkOption {
            description = ''
              Script after dependencies. See <literal>/lib/dag.nix</literal>
            '';
            type = with types; listOf str;
          };
        };
      });
      apply = x: nglib.dag.dagTopoSort x;
      default = { };
    };
    activationScript = mkOption {
      description = ''
        Script generated from <option>system.activation</option>, used to setup the environment.
      '';
      type = types.path;
      readOnly = true;
    };

    name = mkOption {
      description = "System name, used when generating container images";
      type = types.str;
    };
  };

  config = {
    system.activation.currentSystem = nglib.dag.dagEntryAnywhere
      ''
        export PATH=${pkgs.busybox}/bin

        mkdir -p /run
        ln -s $_system_config /run/current-system

        mkdir -p /run/current-system/sw/bin
        ${concatStringsSep "\n" (map (pkg:
          ''
            execs=$(${pkgs.busybox}/bin/find ${pkg}/bin -type f)
            for exec in $execs; do
              cat << EOF > /run/current-system/sw/bin/$(basename $exec)
#!${pkgs.busybox}/bin/sh

set -n
source /etc/profile
exec $exec "\$@"
EOF
              chmod +x /run/current-system/sw/bin/$(basename $exec)
            done
          ''
        ) config.environment.systemPackages)}
      '';

    system.activationScript = pkgs.writeShellScript "activation"
      ''
        ## Set path to the system closure
        ## This is substituted in from `top-level`
        _system_config="@systemConfig@"

        _status=0
        trap "_status=1 _localstatus=\$?" ERR

        ${concatStringsSep "\n" (map (dag:
          ''
            _localstatus=0
            echo "Running activation script ${dag.name}"
            (
              unset PATH
              ${dag.data}
            )
            if (( _localstatus > 0 )); then
              printf "Activation script snippet '%s' failed (%s)\n" "${dag.name}" "$_localstatus"
            fi
          ''
        ) cfg.activation.result)}

        exit $_status
      '';

    assertions = [
      {
        assertion = !(cfg.activation ? "cycle" || cfg.activation ? "loops");
        message = ''
          `cfg.activation` has one or more cycles and/or loops.
          - cycles:
            ${(map (x: "{ after = [ ${concatSepStrings " " x.after} ]; data = ${x.data}; name = ${x.name} }") cfg.activation.loops or []) or ""}
          - loops:
            ${(map (x: "{ after = [ ${concatSepStrings " " x.after} ]; data = ${x.data}; name = ${x.name} }") cfg.activation.loops or [])}
        '';
      }
    ];
  };
}
