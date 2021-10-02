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

{ config, nglib, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.crond;
in
{
  options.services.crond = {
    enable = mkEnableOption "Enable crond, the daemon to execute scheduled commands, specifically cronie.";
    package = mkOption {
      type = types.package;
      description = "The package which contains a cron implementation.";
      default = pkgs.cronie;
    };

    crontabs = mkOption {
      type = with types; attrsOf (submodule {
        options = {
          environment = mkOption {
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
          jobs = mkOption {
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

      apply = x:
        let
          cronfiles =
            (mapAttrsToList
              (n: v: pkgs.writeText n
                ''
                  ${concatStringsSep "\n" (mapAttrsToList (n: v: ''"${n}"="${v}"'') v.environment)}
                  ${concatStringsSep "\n" v.jobs}
                '')
              x);
        in
        pkgs.runCommandNoCCLocal "cron.d" { } ''
          CRONFILES="${concatStringsSep " " cronfiles}"
          mkdir -p $out
          for cronfile in $CRONFILES ; do
            ln -s "$cronfile" $out/$(basename "$cronfile")
          done
        '';
    };
  };

  config = mkIf cfg.enable {
    system.activation."crond" = nglib.dag.dagEntryAnywhere
      ''
        export PATH=${pkgs.busybox}/bin

        mkdir -p /etc /var/run /var/spool/cron /var/cron
        ln -s ${cfg.crontabs}/ /etc/cron.d
      '';

    init.services.crond =
      {
        script = ''
          ${cfg.package}/bin/crond -n -x ext,sch,misc
        '';
        enabled = true;
      };
  };
}
