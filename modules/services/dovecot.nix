# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

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
in
{
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

      config = lib.mkOption {
        type =
          with lib.types;
          let
            self = attrsOf (
              nullOr (oneOf [
                str
                int
                package
                bool
                (listOf (oneOf [
                  str
                  int
                  package
                  bool
                ]))
                (attrsOf self)
              ])
            );
          in
          self // { description = "loop breaker"; };
        description = "Dovecot configuration entries in Nix format.";
        default = { };
        apply = x: pkgs.writeText "dovecot.conf" (nglib.generators.toDovecot x);
      };

      extConfig = lib.mkOption {
        type =
          with lib.types;
          let
            self = attrsOf (
              nullOr (oneOf [
                str
                int
                package
                bool
                (listOf (oneOf [
                  str
                  int
                  package
                  bool
                ]))
                (attrsOf self)
              ])
            );
          in
          self // { description = "loop breaker"; };
        description = "Extra config files to generate, if you pass in a config attrset, you can access the generated file via the `config.services.dovecot.extConfig.<name>` attribute.";
        default = { };
        apply = x: lib.mapAttrs (n: v: pkgs.writeText n (nglib.generators.toDovecot v)) x;
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
      config = {
        default_login_user = lib.mkIf (cfg.loginUser != null) cfg.loginUser;
        default_internal_user = lib.mkIf (cfg.user != null) cfg.user;
        default_internal_group = lib.mkIf (cfg.group != null) cfg.group;

        auth_mechanisms = lib.mkDefault "plain";

        namespace."inbox" = {
          inbox = true;

          mailbox."Drafts" = {
            special_use = "\\Drafts";
          };

          mailbox."Junk" = {
            special_use = "\\Junk";
          };

          mailbox."Trash" = {
            special_use = "\\Trash";
          };

          mailbox."Sent" = {
            special_use = "\\Sent";
          };

          mailbox."Sent Messages" = {
            special_use = "\\Sent";
          };
        };
      };
    };

    init.services.dovecot = {
      ensureSomething.link."modules" = lib.mkDefault {
        src = modulesDir;
        dst = "/etc/dovecot/modules";
        persistent = false;
      };

      ensureSomething.link."config" = lib.mkDefault {
        src = cfg.config;
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
