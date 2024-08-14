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

  secretModule = {name, ...}: {
    options = {
      source = mkOption {
        type = types.attrTag {
          file = mkOption {
            type = types.path;
          };

          environment = mkOption {
            type = types.str;
          };
        };
      };

      generate = mkOption {
        type = types.str;
        default = "false";
      };

      placeholder = mkOption {
        type = types.str;
        default = name;
      };
    };
  };

  case = a: attrs:
    attrs.${a};

  tagCase = a: attrs:
    attrs.${head (attrNames a)};

  fromMaybe = maybe: value:
    if maybe == null then
      value
    else
      maybe;

  declareSubstituteSecrets =
    secrets:
    {
      targetNotFound ? "_targetNotFound_default",
      secretNotUsed ? "_secretNotUsed_default",
      secretNotFound ? "_secretNotFound_default",
    }:
    ''
      function _targetNotFound_default() {
        local _target="$1"

        echo "$_target not found"
        exit 2
      }

      function _secretNotUsed_default() {
        local _placeholder="$2"

        echo "$_placeholder not used"
        exit 2
      }

      function _secretNotFound_default() {
        local _placeholder="$2"

        echo "$_placeholder was not found"
        exit 2
      }

      function substituteSecrets()
      {
        local _target=$1

        if ! [ -f "$_target" ] ; then
          ${targetNotFound} "$_target"
        fi

        ${concatMapStringsSep "\n" (secret:
          ''
            local _placeholder="${secret.placeholder}"
            grep "@$_placeholder@" "$_target" || ${secretNotUsed} "$_target" "$_placeholder"

            unset _secret
            local _secret
            ${tagCase secret.source {
              file = ''
                if [ -f "${secret.source.file}" ] \
                  || ${pkgs.writeShellScript "gitea-generate-${secret.placeholder}" secret.generate} "${secret.source.file}" \
                   ; then
                    _secret="$(cat "${secret.source.file}" | head -n 1)"
                else
                  ${secretNotFound} "$_target" "$_placeholder"
                fi
              '';
              environment = ''
                _secret=${secret.source.environment}
                _secret=''${!_secret}
              '';
            }}

            sed -i "s,@${secret.placeholder}@,$_secret,g" "$_target"
          ''
        ) (attrValues secrets)}
      }
    '';
in
{
  imports = [
    (mkRemovedOptionModule [ "services" "gitea" "appName" ] "The option has been moved to <services.gitea.settings.default.appName")
    (mkRemovedOptionModule [ "services" "gitea" "runMode" ] "The option has been moved to <services.gitea.settings.default.runMode")
    (mkRemovedOptionModule [ "services" "gitea" "user" ] "The option has been moved to <services.gitea.settings.default.runUser")
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
      type = types.attrsOf (types.submodule secretModule);
      default = { };
    };

    settings = mkOption {
      description = ''
        Gitea configuration.
      '';
      type = configFormat.type;
      example = ''
        {
          repository = { ROOT = /data/gitea/git/repositories; };
          repository.local = { LOCAL_COPY_PATH = /data/gitea/tmp/local-repo; };
        }
      '';
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
    services.gitea.settings = {
      server.STATIC_ROOT_PATH = "${cfg.package.data}";
    };

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
        owner = cfg.settings.default.RUN_USER or "root";
        persistent = false;
        dst = cfg.runConfig;
      };

      script = pkgs.writeShellScript "gitea-run"
        (
          let
            appIni = configFormat.generate "app.ini" cfg.settings;
          in
            ''
              set -x
              export PATH=${pkgs.busybox}/bin:${pkgs.bash}/bin

              cp ${appIni} ${cfg.runConfig}
              chmod 600 ${cfg.runConfig}
              chown ${cfg.settings.default.RUN_USER}:nogroup ${cfg.runConfig}

              ${declareSubstituteSecrets cfg.secrets { secretNotFound = "false"; }}
              substituteSecrets ${cfg.runConfig}
              cat ${cfg.runConfig}

              ${optionalString (cfg.package.name == pkgs.forgejo.name) ''
                mkdir -p ${cfg.settings.server.APP_DATA_PATH}/conf
                ln -sf ${cfg.package}/locasle ${cfg.settings.server.APP_DATA_PATH}/conf
              ''}

              export HOME=${cfg.settings.server.APP_DATA_PATH}
              chpst -u ${cfg.settings.default.RUN_USER or "root"}:nogroup ${cfg.package}/bin/gitea -c ${cfg.runConfig}
            ''
        );

      enabled = true;
    };

    environment.systemPackages = [ cfg.package ];

    users.users."gitea" = mkIf ((cfg.settings.default.RUN_USER or "root") == defaultUser) {
      uid = ids.uids.gitea;
      description = "Gitea user";
    };
  };
}
