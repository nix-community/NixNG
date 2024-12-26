# SPDX-FileCopyrightText:  2024 Richard Brežák, NixNG contributors, NixOS contributors
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
  cfg = config.dinit;

  generateUserCommandsScript =
    name: service:
    pkgs.writeShellScript "${name}-user-commands.sh" ''
      set -eo pipefail

      ${lib.optionalString (service.execStartPre != null) service.execStartPre}
      ${service.execStart}
      ${lib.optionalString (service.execStop != null) service.execStop}
    '';

  generateDependsOn = lib.concatMapStringsSep "\n" (dep: "depends-on: ${dep}");

  generateEnvFile =
    { name, environment, ... }:
    pkgs.writeText "${name}.env" (
      lib.concatMapStringsSep "\n" (var: var.name + "=" + var.value) (
        lib.mapAttrsToList lib.nameValuePair (lib.filterAttrs (n: _: !lib.elem n [ "PATH" ]) environment)
      )
    );

  generateServiceFile =
    { name, service }:
    let
      rules = (nglib.nottmpfiles.ensureSomethings service.ensureSomething) ++ service.tmpfiles;
      rulesFile = pkgs.writeText "${name}.tmpfiles" (nglib.nottmpfiles.generate rules);
    in
    pkgs.writeText "${name}-service" ''
      type = process
      pid-file = /service/${name}/pid

      command = ${pkgs.writeShellScript "${name}-start.sh" ''
        set -eo pipefail

        export PATH="${
          lib.concatStringsSep ":" (
            [ "$PATH" ] ++ (lib.optional (service.environment ? PATH) service.environment."PATH")
          )
        }"

        ${pkgs.systemdTmpfilesD}/bin/systemd-tmpfiles --create ${rulesFile}
        ${nglib.maybeChangeUserAndGroup service.user service.group (
          generateUserCommandsScript name service
        )}
        ${pkgs.systemdTmpfilesD}/bin/systemd-tmpfiles --remove ${rulesFile}
      ''}

      logfile = /proc/1/fd/2
      env-file =
      ${lib.optionalString (service.environmentFile != null) ''
        env-file += ${service.environmentFile}
      ''}
      env-file += ${
        generateEnvFile {
          inherit name;
          inherit (service) environment;
        }
      }
      working-dir = ${service.workingDirectory}
      ${generateDependsOn service.dependencies}
    '';

  bootService = pkgs.writeText "dinit-boot-service" ''
    type = internal

    restart = no

    ${lib.pipe config.init.services [
      (lib.mapAttrsToList (name: service: service // { inherit name; }))
      (lib.filter (service: service.enabled && service.shutdownOnExit))
      (lib.map (service: "depends-on: " + service.name))
      (lib.concatStringsSep "\n")
    ]}

    ${lib.pipe config.init.services [
      (lib.mapAttrsToList (name: service: service // { inherit name; }))
      (lib.filter (service: service.enabled && !service.shutdownOnExit))
      (lib.map (service: "waits-for: " + service.name))
      (lib.concatStringsSep "\n")
    ]}
  '';
in
{
  options.dinit = {
    enable = lib.mkEnableOption "Enable dinit";

    package = lib.mkPackageOption {
      description = "dinit package to use";
      type = lib.types.package;
      default = pkgs.dinit;
    };

    serviceDirectory = lib.mkOption {
      description = "Generated service directory";
      type = lib.types.path;
      readOnly = true;
    };
  };

  config = lib.mkIf cfg.enable {
    system.activation."dinit" = nglib.dag.dagEntryAnywhere ''
      mkdir /sv

      function linkFarm() {
          src="$1"
          dst="$2"

          find "$src" -mindepth 1 -type d -print0 | sed -e "s~$src~~" | xargs -0 -I {} mkdir "$dst/{}"
          find "$src" -mindepth 1 -type f -print0 | sed -e "s~$src~~" | xargs -0 -I {} ln -s "$src/{}" "$dst/{}"
          find "$src" -mindepth 1 -type l -print0 | sed -e "s~$src~~" | xargs -0 -I {} cp "$src/{}" "$dst/{}"
      }

      linkFarm ${cfg.serviceDirectory} /sv
    '';

    dinit.serviceDirectory = pkgs.runCommandNoCC "dinit-service-directory" { } ''
      mkdir -p $out

      ${lib.concatMapStringsSep "\n" (
        { name, value }:
        ''
          ln -s ${
            generateServiceFile {
              inherit name;
              service = value;
            }
          } $out/${name}
        ''
      ) (lib.mapAttrsToList lib.nameValuePair config.init.services)}

      ln -s ${bootService} $out/boot
    '';

    init = lib.mkMerge [
      { availableInits = [ "dinit" ]; }
      (lib.mkIf cfg.enable {
        type = "dinit";
        script = pkgs.writeShellScript "dinit-start" ''
          export PATH=${pkgs.busybox}/bin:${pkgs.runit}/bin:${pkgs.dinit}/bin \
                 _system_config="@systemConfig@"
          # Run activation script for this system
          "$_system_config/activation"

          . /etc/profile

          exec ${lib.getExe pkgs.dinit} -d /sv -s
        '';

        shutdown = pkgs.writeShellScript "dinit-shutdown" ''
          exec ${lib.getExe pkgs.dinit} shutdown
        '';
      })
    ];
  };
}
