# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.services.gitea;
  ids = config.ids;

  configFormat = pkgs.formats.ini {};

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
  imports = [
    (mkRemovedOptionModule [ "services" "gitea" "appName" ] "The option has been moved to <services.gitea.configuration.appName")
    (mkRemovedOptionModule [ "services" "gitea" "runMode" ] "The option has been moved to <services.gitea.configuration.runMode")
    (mkRemovedOptionModule [ "services" "gitea" "user" ] "The option has been moved to <services.gitea.configuration.runUser")
    (mkRemovedOptionModule [ "services" "gitea" "configuration" ] "The option has been renamed to <services.gitea.settings>")
  ];

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

    settings = mkOption {
      description = ''
        Gitea configuration.
      '';
      type = with types; submodule {
        freeformType = configFormat.type;

        options = {
          appName = mkOption {
            description = ''
              Gitea's app name, displayed on the front page.
            '';
            type = types.str;
            default = "Gitea";
          };
          runMode = mkOption {
            description = ''
              Gitea's run mode.
            '';
            type = types.str;
            default = "prod";
          };
          runUser = mkOption {
            description = ''
              Gitea's user. Under which the Gitea process runs.
            '';
            type = types.str;
            default = defaultUser;
          };
        };
      };
      example = ''
        {
          repository = { ROOT = /data/gitea/git/repositories; };
          repository.local = { LOCAL_COPY_PATH = /data/gitea/tmp/local-repo; };
        }
      '';
      default = {
        server.STATIC_ROOT_PATH = "${pkgs.gitea.data}";
      };
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

  config = mkIf cfg.enable {
    init.services.gitea = {
      ensureSomething.create."0-server.APP_DATA_PATH" = {
        type = "directory";
        mode = "755";
        owner = "gitea:nogroup";
        persistent = true;
        dst = cfg.settings."server"."APP_DATA_PATH";
      };

      ensureSomething.create."runConfig" = {
        type = "file";
        mode = "600";
        owner = "gitea:nogroup";
        persistent = false;
        dst = cfg.runConfig;
      };

      script = pkgs.writeShellScript "gitea-run"
        (
          let
            appIni = pkgs.writeText "app.ini"
              ''
                APP_NAME = ${cfg.settings.appName}
                RUN_MODE = ${cfg.settings.runMode}
                RUN_USER = ${cfg.settings.runUser}

                ${generators.toINI {} (filterAttrs (n: _: !(elem n ["appName" "runMode" "runUser"])) cfg.settings)}
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
            chmod 600 ${cfg.runConfig}
            chown ${cfg.settings.runUser}:nogroup ${cfg.runConfig}

            ${subsSecret secretKeyFile "secretKey"}
            ${subsSecret internalTokenFile "internalToken"}
            ${subsSecret jwtSecretFile "jwtSecret"}
            ${subsSecret lfsJwtSecretFile "lfsJwtSecret"}
            ${subsSecret databaseUserFile "databaseUser"}
            ${subsSecret databasePasswordFile "databasePassword"}
            ${subsSecret databaseHostFile "databaseHost"}

            ${optionalString (cfg.package.name == pkgs.forgejo.name) ''
              set -v
              mkdir -p ${cfg.settings.server.APP_DATA_PATH}/conf
              ln -sf ${cfg.package}/locale ${cfg.settings.server.APP_DATA_PATH}/conf
              ls ${cfg.settings.server.APP_DATA_PATH}/conf/locale
            ''}

            export HOME=${cfg.settings.server.APP_DATA_PATH}
            chpst -u ${cfg.settings.runUser}:nogroup ${cfg.package}/bin/gitea -c ${cfg.runConfig}
          ''
        );

      enabled = true;
    };

    environment.systemPackages = [ cfg.package ];

    users.users."gitea" = mkIf (cfg.settings.runUser == defaultUser) {
      uid = ids.uids.gitea;
      description = "Gitea user";
    };
  };
}
