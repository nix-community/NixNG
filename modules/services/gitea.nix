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

{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.services.gitea;
  ids = config.ids;

  defaultUser = "gitea";

  giteaSecrets = {
    options = {
      secretKeyFile = mkOption {
        description = ''
          Path to a file containing Gitea's secret key, on one line.
          If non-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place of
          <literal>#secretKey#</literal>.
        '';
        type = with types; nullOr path;
        default = null;
      };
      internalTokenFile = mkOption {
        description = ''
          Path to a file containing Gitea's internal token, on one line.
          If non-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place of
          <literal>#internalToken#</literal>.
        '';
        type = with types; nullOr path;
        default = null;
      };
      jwtSecretFile = mkOption {
        description = ''
          Path to a file containing Gitea's JWT secret, on one line.
          If non-<literal>null</literal>, the contents of this file
          will substituted into the generated configuration file in-place
          of <literal>#jwtSecret#</literal>.
        '';
        type = with types; nullOr path;
        default = null;
      };
      lfsJwtSecretFile = mkOption {
        description = ''
          Path to a file containing Gitea's LFS JWT secret, on one line.
          If non-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place of
          <literal>#lfsJwtSecret#</literal>.
        '';
        type = with types; nullOr path;
        default = null;
      };

      databaseUserFile = mkOption {
        description = ''
          Path to a file containing user with which Gitea should connect
          to it's database, on one line. If non-<literal>null</literal>,
          the contents of this file will substituted into the generated
          configuration file in-place of <literal>#databaseUser#</literal>.
        '';
        type = with types; nullOr path;
        default = null;
      };
      databasePasswordFile = mkOption {
        description = ''
          Path to a file containing password with which Gitea should
          connect to it's database, on one line. If
          on-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place
          of <literal>#databasePassword#</literal>.
        '';
        type = with types; nullOr path;
        default = null;
      };
      databaseHostFile = mkOption {
        description = ''
          Path to a file containing database host to which Gitea should
          connect, on one line. If non-<literal>null</literal>, the
          contents of this file will substituted into the generated
          configuration file in-place of <literal>#databaseHost#</literal>.
        '';
        type = with types; nullOr path;
        default = null;
      };
    };
  };
in
{
  options.services.gitea = {
    enable = mkEnableOption "Enable Gitea service.";

    package = mkOption {
      description = "Gitea package.";
      type = types.package;
      default = pkgs.gitea;
    };

    secrets = mkOption {
      description = "Gitea secrets.";
      type = types.submodule giteaSecrets;
      default = { };
    };

    configuration = mkOption {
      description = ''
        Gitea configuration. <literal>APP_NAME</literal>,
        <literal>RUN_MODE</literal>, and <literal>RUN_USER</literal>
        are specified separately, because they are not under any section,
        therefore the Gitea configuration file isn't a real INI file.
      '';
      type = types.attrs;
      example = ''
        {
          repository = { ROOT = /data/gitea/git/repositories; };
          repository.local = { LOCAL_COPY_PATH = /data/gitea/tmp/local-repo; };
        }
      '';
    };

    appName = mkOption {
      description = ''
        Gitea's app name, displayed on the front page.
        As to why this is explicitly set here, see
        <option>services.gitea.configuration</option>.
      '';
      type = types.str;
      default = "Gitea";
    };
    runMode = mkOption {
      description = ''
        Gitea's run mode.
        As to why this is explicitly set here, see
        <option>services.gitea.configuration</option>.
      '';
      type = types.str;
      default = "prod";
    };
    user = mkOption {
      description = ''
        Gitea's user. Under which the Gitea process runs.
      '';
      type = types.str;
      default = defaultUser;
    };

    runConfig = mkOption {
      description = ''
        Path to Gitea's runtime generated configuration, with secrets.
      '';
      type = types.path;
      default = "/var/run/gitea/app.ini";
    };
    # add explicit path settings and ensure them, also add to config but only as defaults
  };

  config = {
    init.services.gitea = mkIf cfg.enable {
      ensureSomething.create."0-server.APP_DATA_PATH" = {
        type = "directory";
        mode = "755";
        owner = "gitea:nogroup";
        persistent = true;
        dst = cfg.configuration."server"."APP_DATA_PATH";
      };

      ensureSomething.create."runConfig" = {
        type = "file";
        mode = "400";
        owner = "gitea:nogroup";
        persistent = false;
        dst = cfg.runConfig;
      };

      script = pkgs.writeShellScript "gitea-run"
        (
          let
            appIni = pkgs.writeText "app.ini"
              ''
                APP_NAME = ${cfg.appName}
                RUN_MODE = ${cfg.runMode}
                RUN_USER = ${cfg.user}

                ${generators.toINI {} cfg.configuration}
              '';
            inherit (cfg.secrets) secretKeyFile internalTokenFile jwtSecretFile lfsJwtSecretFile databaseUserFile databasePasswordFile databaseHostFile;

            subsSecret = source: key:
              optionalString (source != null)
                ''
                  if [[ -f '${source}' ]] ; then
                    SECRET="$(head -n1 ${source})"
                    sed -i "s,#${key}#,$SECRET,g" ${cfg.runConfig}
                    echo 'Substituted contents of `${source}` in place of `#${key}#`'
                  fi
                '';
          in
          ''
            export PATH=${pkgs.busybox}/bin:${pkgs.bash}/bin

            cp ${appIni} ${cfg.runConfig}

            ${subsSecret secretKeyFile "secretKey"}
            ${subsSecret internalTokenFile "internalToken"}
            ${subsSecret jwtSecretFile "jwtSecret"}
            ${subsSecret lfsJwtSecretFile "lfsJwtSecret"}
            ${subsSecret databaseUserFile "databaseUser"}
            ${subsSecret databasePasswordFile "databasePassword"}
            ${subsSecret databaseHostFile "databaseHost"}

            export HOME=${cfg.configuration.repository.ROOT}
            chpst -u ${cfg.user}:nogroup ${cfg.package}/bin/gitea -c ${cfg.runConfig}
          ''
        );

      enabled = true;
    };

    users.users."gitea" = mkIf (cfg.enable && cfg.user == defaultUser) {
      uid = ids.uids.gitea;
      description = "Gitea user";
    };
  };
}
