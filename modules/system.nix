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

    build = {
      toplevel = mkOption {
        description = ''
          The full system, built up.
        '';
        type = types.path;
      };

      ociImage = mkOption {
        description = ''
          OCI compatible image.
        '';
        type = types.submodule {
          options = {
            build = mkOption {
              description = ''
                A path to a OCI image in a gziped tarball.
              '';
              type = types.path;
            };
            stream = mkOption {
              description = ''
                A script which builds an OCI image and outputs what it builds
                into stdout without saving to disk.
              '';
              type = types.path;
            };
          };
        };
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
      default = "nixng";
      type = types.str;
    };
  };

  config = {
    system.build = {
      toplevel = pkgs.runCommandNoCC "nixng"
        { nativeBuildInputs = with pkgs; [ busybox makeWrapper ]; }
        (with configFinal;
        let
          closureInfo = pkgs.closureInfo
            { rootPaths = [ configFinal.init.script system.activationScript ]; };
        in
        ''
          mkdir $out

          # Substitute in the path to the system closure to avoid
          # an infinite dep cycle
          substitute ${init.script} $out/init \
            --subst-var-by "systemConfig" "$out"
          substitute ${system.activationScript} $out/activation \
            --subst-var-by "systemConfig" "$out"
          chmod +x $out/init $out/activation

          #
          ${optionalString system.createNixRegistration
            "ln -s ${closureInfo}/registration $out/registration"}
        '');

      ociImage =
        let
          config = {
            name = cfg.name;
            tag = "latest";
            maxLayers = 125;

            config = {
              StopSignal = "SIGCONT";
              Entrypoint =
                [
                  "${configFinal.system.build.toplevel}/init"
                ];
            };
          };
        in
        with pkgs; {
          build = dockerTools.buildLayeredImage config;
          stream = dockerTools.streamLayeredImage config;
        };
    };

    system.activation.currentSystem = nglib.dag.dagEntryAnywhere
      ''
        export PATH=${pkgs.busybox}/bin

        mkdir -p /run
        ln -s $_system_config /run/current-system

        mkdir -p /run/current-system/sw/bin
        ${concatStringsSep "\n" (map (pkg:
          ''
            execs=${pkgs.findutils}/bin/find ${pkg}/bin -type f
            for exec in $execs; do
              ln -sf $exec /run/current-system/sw/bin
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
