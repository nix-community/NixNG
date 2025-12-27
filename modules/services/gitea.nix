# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
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
  ...
}:
let
  cfg = config.services.gitea;
  ids = config.ids;

  configFormat = pkgs.formats.ini { };

  defaultUser = "gitea";

  giteaSecrets = {
    options = {
      secretKeyFile = lib.mkOption {
        description = ''
          Path to a file containing Gitea's secret key, on one line.
          If non-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place of
          <literal>#secretKey#</literal>.
        '';
        type = with lib.types; nullOr path;
        default = null;
      };
      internalTokenFile = lib.mkOption {
        description = ''
          Path to a file containing Gitea's internal token, on one line.
          If non-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place of
          <literal>#internalToken#</literal>.
        '';
        type = with lib.types; nullOr path;
        default = null;
      };
      jwtSecretFile = lib.mkOption {
        description = ''
          Path to a file containing Gitea's JWT secret, on one line.
          If non-<literal>null</literal>, the contents of this file
          will substituted into the generated configuration file in-place
          of <literal>#jwtSecret#</literal>.
        '';
        type = with lib.types; nullOr path;
        default = null;
      };
      lfsJwtSecretFile = lib.mkOption {
        description = ''
          Path to a file containing Gitea's LFS JWT secret, on one line.
          If non-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place of
          <literal>#lfsJwtSecret#</literal>.
        '';
        type = with lib.types; nullOr path;
        default = null;
      };

      databaseUserFile = lib.mkOption {
        description = ''
          Path to a file containing user with which Gitea should connect
          to it's database, on one line. If non-<literal>null</literal>,
          the contents of this file will substituted into the generated
          configuration file in-place of <literal>#databaseUser#</literal>.
        '';
        type = with lib.types; nullOr path;
        default = null;
      };
      databasePasswordFile = lib.mkOption {
        description = ''
          Path to a file containing password with which Gitea should
          connect to it's database, on one line. If
          on-<literal>null</literal>, the contents of this file will
          substituted into the generated configuration file in-place
          of <literal>#databasePassword#</literal>.
        '';
        type = with lib.types; nullOr path;
        default = null;
      };
      databaseHostFile = lib.mkOption {
        description = ''
          Path to a file containing database host to which Gitea should
          connect, on one line. If non-<literal>null</literal>, the
          contents of this file will substituted into the generated
          configuration file in-place of <literal>#databaseHost#</literal>.
        '';
        type = with lib.types; nullOr path;
        default = null;
      };
    };
  };

  secretModule =
    { name, ... }:
    {
      options = {
        source = lib.mkOption {
          type = lib.types.attrTag {
            file = lib.mkOption { type = lib.types.path; };

            environment = lib.mkOption { type = lib.types.str; };
          };
        };

        generate = lib.mkOption {
          type = lib.types.str;
          default = "false";
        };

        placeholder = lib.mkOption {
          type = lib.types.str;
          default = name;
        };
      };
    };

  case = a: attrs: attrs.${a};

  tagCase = a: attrs: attrs.${lib.head (lib.attrNames a)};

  fromMaybe = maybe: value: if maybe == null then value else maybe;

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

        ${lib.concatMapStringsSep "\n" (secret: ''
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
        '') (lib.attrValues secrets)}
      }
    '';
in
{
  imports = [
    (lib.mkRemovedOptionModule [
      "services"
      "gitea"
      "appName"
    ] "The option has been moved to <services.gitea.settings.default.appName")
    (lib.mkRemovedOptionModule [
      "services"
      "gitea"
      "runMode"
    ] "The option has been moved to <services.gitea.settings.default.runMode")
    (lib.mkRemovedOptionModule [
      "services"
      "gitea"
      "user"
    ] "The option has been moved to <services.gitea.settings.default.runUser")
    (lib.mkRemovedOptionModule [
      "services"
      "gitea"
      "configuration"
    ] "The option has been renamed to <services.gitea.settings>")
  ];

  options.services.gitea = {
    enable = lib.mkEnableOption "Enable Gitea service.";

    package = lib.mkOption {
      description = "Gitea package.";
      type = lib.types.package;
      default = pkgs.gitea;
    };

    secrets = lib.mkOption {
      description = "Gitea secrets.";
      type = lib.types.attrsOf (lib.types.submodule secretModule);
      default = { };
    };

    settings = lib.mkOption {
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

    runConfig = lib.mkOption {
      description = ''
        Path to Gitea's runtime generated configuration, with secrets.
      '';
      type = lib.types.path;
      default = "/run/gitea/app.ini";
    };
    # add explicit path settings and ensure them, also add to config but only as defaults
  };

  config = lib.mkIf cfg.enable {
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
        owner =
          if (cfg.settings.default or { }) ? RUN_USER then
            "${cfg.settings.default.RUN_USER}:nogroup"
          else
            "root:root";
        persistent = false;
        dst = cfg.runConfig;
      };

      script = pkgs.writeShellScript "gitea-run" (
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

          ${lib.optionalString (cfg.package.name == pkgs.forgejo.name) ''
            mkdir -p ${cfg.settings.server.APP_DATA_PATH}/conf
            ln -sf ${cfg.package}/locale ${cfg.settings.server.APP_DATA_PATH}/conf
          ''}

          export HOME=${cfg.settings.server.APP_DATA_PATH}
          chpst -u ${
            cfg.settings.default.RUN_USER or "root"
          }:nogroup ${cfg.package}/bin/gitea -c ${cfg.runConfig}
        ''
      );

      enabled = true;
    };

    environment.systemPackages = [ cfg.package ];

    users.users."gitea" = lib.mkIf ((cfg.settings.default.RUN_USER or "root") == defaultUser) {
      uid = ids.uids.gitea;
      description = "Gitea user";
    };
  };
}
