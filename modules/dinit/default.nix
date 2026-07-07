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

  dinitService = nglib.generators.dinitService { };

  wrapEnv =
    {
      name,
      path,
      environment,
      command,
    }:
    pkgs.runCommand "${name}-env-wrapper"
      {
        buildInputs = [ pkgs.makeBinaryWrapper ];
      }
      ''
        makeWrapper ${command} $out \
          ${
            lib.concatMapAttrsStringSep " \\\n" (
              name: value: "--set ${lib.escapeShellArg name} ${lib.escapeShellArg value}"
            ) environment
          } \
          --prefix PATH : "${path}"
      '';

  generateServices = lib.concatMapAttrs (
    name: service:
    let
      doExecStartPre = service.execStartPre != null || rules != [ ];
      doExecStopPost = rules != [ ];
      wrapCommand =
        command:
        let
          envCommand = wrapEnv {
            inherit name;
            path = lib.concatStringsSep ":" (
              [ (pkgs.setgroups + "/bin") ] ++ lib.optional (service.environment ? PATH) service.environment."PATH");
            environment = lib.removeAttrs service.environment [ "PATH" ];
            command = lib.removePrefix "+" command;
          };
        in
          if lib.hasPrefix "+" command then
            envCommand
          else
            nglib.maybeChangeUserAndGroup {
              inherit (pkgs) setgroups;
              inherit (service) user group supplementaryGroups;
              command = envCommand;
            };
      multiCommand = name: commands:
        pkgs.writeShellScript name ''
          set -eEuo pipefail

          ${lib.concatStringsSep "\n" commands}
        '';

      rules = (nglib.nottmpfiles.ensureSomethings service.ensureSomething) ++ service.tmpfiles;
      rulesFile = pkgs.writeText "${name}.tmpfiles" (nglib.nottmpfiles.generate rules);
      filehammerEtcService = "file-hammer_" + nglib.escapeSystemdPath "/etc";
    in
    {
      ${nglib.optionalAttr doExecStartPre "${name}-pre-start"} = {
        type = "scripted";

        logfile = "/proc/1/fd/2";
        working-dir = service.workingDirectory;

        command = multiCommand "${name}-pre-start-command" (
          (lib.optional (rules != []) "${lib.getExe' pkgs.systemdTmpfilesD "systemd-tmpfiles"} --create ${rulesFile}")
        ++ (lib.optional (service.execStartPre != null) (wrapCommand service.execStartPre)));
      };
      "${name}-start" = {
        inherit (service) type;
        pid-file = "/sv/${name}/pid";

        logfile = "/proc/1/fd/2";
        working-dir = service.workingDirectory;

        ${nglib.optionalAttr doExecStartPre "depends-on"} = "${name}-pre-start";
        ${nglib.optionalAttr doExecStopPost "chain-to"} = "${name}-stop-post";

        command = wrapCommand service.execStart;
        ${nglib.optionalAttr' service.execStop "stop-command"} = wrapCommand service.execStop;
      };
      ${nglib.optionalAttr doExecStopPost "${name}-stop-post"} = {
        type = "scripted";

        logfile = "/proc/1/fd/2";
        working-dir = service.workingDirectory;

        command = "${lib.getExe' pkgs.systemdTmpfilesD "systemd-tmpfiles"} --remove ${rulesFile}";
      };
      ${name} = {
        type = "internal";

        depends-on = [
          "${name}-start"
        ]
        ++ service.dependencies
        ++ (lib.optional (name != filehammerEtcService) filehammerEtcService);
      };
    }
  );

  bootService = pkgs.writeText "dinit-boot-service" (
    dinitService.generate {
      type = "internal";

      restart = "no";

      depends-on = lib.pipe config.init.services [
        (lib.filterAttrs (_: service: service.enabled && service.shutdownOnExit))
        lib.attrNames
      ];

      waits-for = lib.pipe config.init.services [
        (lib.filterAttrs (_: service: service.enabled && !service.shutdownOnExit))
        lib.attrNames
      ];
    }
  );
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

  config = lib.mkMerge [
    { init.availableInits = [ "dinit" ]; }
    (lib.mkIf cfg.enable {
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

      dinit.serviceDirectory = pkgs.runCommand "dinit-service-directory" { } ''
        mkdir -p $out

        ${lib.concatMapAttrsStringSep "\n" (name: value: ''
          cat > $out/${name} <<EOF
          ${dinitService.generate value}
          EOF
        '') (generateServices config.init.services)}

        ln -s ${bootService} $out/boot
      '';

      init = {
        type = "dinit";
        script = pkgs.writeShellScript "dinit-start" ''
          export PATH=${pkgs.busybox}/bin:${pkgs.dinit}/bin \
                 _system_config="@systemConfig@"
          # Run activation script for this system
          "$_system_config/activation"

          exec ${lib.getExe pkgs.dinit} \
            --services-dir /sv \
            --system-mgr \
            --container
        '';

        shutdown = pkgs.writeShellScript "dinit-shutdown" ''
          exec ${lib.getExe pkgs.dinit} shutdown
        '';
      };

      environment.systemPackages = [
        pkgs.dinit
      ];
    })
  ];
}
