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

  config = mkIf cfg.enable {
    init.services.gitea = {
      ensureSomething.create."0-server.APP_DATA_PATH" = {
        type = "directory";
        mode = "755";
        owner = "gitea:nogroup";
        persistent = true;
        dst = cfg.configuration."server"."APP_DATA_PATH";
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
            chmod 600 ${cfg.runConfig}
            chown ${cfg.user}:nogroup ${cfg.runConfig}

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

    environment.systemPackages = [ cfg.package ];

    users.users."gitea" = mkIf (cfg.user == defaultUser) {
      uid = ids.uids.gitea;
      description = "Gitea user";
    };
  };
}
