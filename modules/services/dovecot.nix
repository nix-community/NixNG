# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  pkgs,
  config,
  lib,
  nglib,
  ...
}:
let
  cfg = config.services.dovecot;

  modulesDir = pkgs.symlinkJoin {
    name = "dovecot-modules";
    paths = map (pkg: "${pkg}/lib/dovecot/modules") (
      [ cfg.package ] ++ map (module: module.override { dovecot = cfg.package; }) cfg.modules
    );
  };

  inherit (config) ids;

  dovecot = nglib.generators.dovecot { inherit (cfg.settings) dovecot_config_version; };
in
{
  imports = [
    (lib.mkRenamedOptionModule [ "services" "dovecot" "config" ] [ "services" "dovecot" "settings" ])
  ];

  options = {
    services.dovecot = {
      enable = lib.mkEnableOption "Enable Dovecot.";

      package = lib.mkOption {
        description = "Dovecot package.";
        type = lib.types.package;
        default = pkgs.dovecot;
      };

      user = lib.mkOption {
        description = "Dovecot user.";
        type = lib.types.str;
        default = "dovecot";
      };

      group = lib.mkOption {
        description = "Dovecot group.";
        type = lib.types.str;
        default = "dovecot";
      };

      mailUser = lib.mkOption {
        description = "Dovecot mail user.";
        type = lib.types.nullOr lib.types.str;
        default = "vmail";
      };

      mailGroup = lib.mkOption {
        description = "Dovecot mail group.";
        type = with lib.types; nullOr str;
        default = "vmail";
      };

      loginUser = lib.mkOption {
        description = "Dovecot user for untrusted logins, should not have access to anything.";
        type = with lib.types; nullOr str;
        default = "dovenull";
      };

      modules = lib.mkOption {
        type = with lib.types; listOf package;
        default = [ ];
        example = lib.literalExample "[ pkgs.dovecot_pigeonhole ]";
        description = ''
          Symlinks the contents of lib/dovecot of every given package into
          the haskell modules directory. This will make the given modules available
          if a dovecot package with the module_dir patch applied is being used.
        '';
      };

      settings = lib.mkOption {
        type = lib.types.submodule {
          freeformType = dovecot.type;

          options.dovecot_config_version = lib.mkOption {
            type = lib.types.str;
            description = ''
              Dovecot configuration version. It uses the same versioning as Dovecot in general,
              e.g. `3.0.5`. This must be the first setting in the configuration file. It specifies
              the configuration syntax, the used setting names and the expected default values.

              When there are default configuration changes in newer Dovecot versions, the existing
              installations will continue to work the same as before with the same default settings
              until this version number is increased. If there are other configuration changes, the
              old configuration will either keep working or there will be a clear failure at startup.
            '';
          };

          options.dovecot_storage_version = lib.mkOption {
            type = lib.types.str;
            description = ''
              Dovecot storage file format version. It uses the same versioning as Dovecot in general,
              e.g. `3.0.5`. It specifies the oldest Dovecot version that must be able to read files
              written by this Dovecot instance. The intention is that when upgrading Dovecot cluster,
              this setting is first kept as the old Dovecot version. Once the cluster is fully upgraded
              to a new version and there is no intention to rollback to the old version anymore, this
              version number can be increased.
            '';
          };
        };
        description = "Dovecot configuration entries in Nix format.";
        default = { };
      };

      configFile = lib.mkOption {
        type = lib.types.path;
        description = "Dovecot config file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    users.users."dovecot" = lib.mkIf (cfg.user == "dovecot") {
      uid = ids.uids.dovecot;
      description = "Dovecot user.";
      group = "dovecot";
    };
    users.groups."dovecot" = lib.mkIf (cfg.group == "dovecot") { gid = ids.gids.dovecot; };
    users.users."vmail" = lib.mkIf (cfg.mailUser == "vmail") {
      uid = ids.uids.vmail;
      description = "vmail user.";
      group = "vmail";
    };
    users.groups."vmail" = lib.mkIf (cfg.mailGroup == "vmail") { gid = ids.gids.vmail; };
    users.users."dovenull" = lib.mkIf (cfg.loginUser == "dovenull") {
      uid = ids.uids.dovenull;
      description = "Dovecot untrusted login user.";
      group = "dovenull";
    };
    users.groups."dovenull" = lib.mkIf (cfg.loginUser == "dovenull") { gid = ids.gids.dovenull; };

    environment.systemPackages = [ cfg.package ];

    services.dovecot = {
      configFile = lib.mkDefault (pkgs.writeText "dovecot.conf" (dovecot.generate cfg.settings));
      config = {
        default_login_user = lib.mkIf (cfg.loginUser != null) cfg.loginUser;
        default_internal_user = lib.mkIf (cfg.user != null) cfg.user;
        default_internal_group = lib.mkIf (cfg.group != null) cfg.group;
      };
    };

    init.services.dovecot = {
      ensureSomething.link."modules" = lib.mkDefault {
        src = modulesDir;
        dst = "/etc/dovecot/modules";
        persistent = false;
      };

      ensureSomething.link."config" = lib.mkDefault {
        src = cfg.configFile;
        dst = "/etc/dovecot/dovecot.conf";
        persistent = false;
      };

      script = pkgs.writeShellScript "dovecot-run" ''
        echo ${cfg.package}/sbin/dovecot -F
        ${cfg.package}/sbin/dovecot -F
      '';
      enabled = true;
    };
  };
}
