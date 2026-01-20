# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  config,
  nglib,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.crond;
in
{
  options.services.crond = {
    enable = lib.mkEnableOption "Enable crond, the daemon to execute scheduled commands, specifically cronie.";
    package = lib.mkOption {
      type = lib.types.package;
      description = "The package which contains a cron implementation.";
      default = pkgs.cronie;
    };

    crontabs = lib.mkOption {
      type =
        with lib.types;
        attrsOf (submodule {
          options = {
            environment = lib.mkOption {
              type = attrsOf str;
              description = ''
                A set of environment variable to set for this specific crontab.
              '';
              example = ''
                {
                  MAILTO="root";
                }
              '';
              default = { };
            };
            jobs = lib.mkOption {
              type = listOf str;
              description = ''
                A list of job entries, in the usual cron format.
              '';
              example = ''
                [
                  "5 0 * * * www-data rm /var/log/{httpd.access,httpd.error}"
                ]
              '';
              default = [ ];
            };
          };
        });
      description = ''
        Defines cron jobs, allows for the creation of multiple files and entries.
      '';
      default = { };
      example = ''
        {
          "delete-log" = {
            environment = {
              MAILTO="root";
            };
            jobs = [
              "5 0 * * * www-data rm /var/log/{httpd.access,httpd.error}"
            ];
          };
        }
      '';

      apply =
        x:
        let
          cronfiles = (
            lib.mapAttrsToList (
              n: v:
              pkgs.writeText n ''
                ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: ''"${n}"="${v}"'') v.environment)}
                ${lib.concatStringsSep "\n" v.jobs}
              ''
            ) x
          );
        in
        pkgs.runCommand "cron.d" { } ''
          CRONFILES="${lib.concatStringsSep " " cronfiles}"
          mkdir -p $out
          for cronfile in $CRONFILES ; do
            ln -s "$cronfile" $out/$(basename "$cronfile")
          done
        '';
    };
  };

  config = lib.mkIf cfg.enable {
    system.activation."crond" = nglib.dag.dagEntryAnywhere ''
      export PATH=${pkgs.busybox}/bin

      mkdir -p /etc /var/run /var/spool/cron /var/cron
      ln -s ${cfg.crontabs}/ /etc/cron.d
    '';

    init.services.crond = {
      script = ''
        ${cfg.package}/bin/crond -n -x ext,sch,misc
      '';
      enabled = true;
    };

    environment.systemPackages = [ cfg.package ];
  };
}
