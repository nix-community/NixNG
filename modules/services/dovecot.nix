# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, nglib, ... }:
with lib;
let
  cfg = config.services.dovecot;

  modulesDir = pkgs.symlinkJoin {
    name = "dovecot-modules";
    paths = map (pkg: "${cfg.package}/lib/dovecot")
      ([ cfg.package ] ++ map (module: module.override { dovecot = cfg.package; }) cfg.modules);
  };

  inherit (config) ids;
in
{
  options = {
    services.dovecot = {
      enable = mkEnableOption "Enable Dovecot.";

      package = mkOption {
        description = "Dovecot package.";
        type = types.package;
        default = pkgs.dovecot;
      };

      user = mkOption {
        description = "Dovecot user.";
        type = types.str;
        default = "dovecot";
      };

      group = mkOption {
        description = "Dovecot group.";
        type = types.str;
        default = "dovecot";
      };

      mailUser = mkOption {
        description = "Dovecot mail user.";
        type = types.nullOr types.str;
        default = "vmail";
      };

      mailGroup = mkOption {
        description = "Dovecot mail group.";
        type = types.nullOr types.str;
        default = "vmail";
      };

      loginUser = mkOption {
        description = "Dovecot user for untrusted logins, should not have access to anything.";
        type = types.nullOr types.str;
        default = "dovenull";
      };

      modules = mkOption {
        type = types.listOf types.package;
        default = [ ];
        example = literalExample "[ pkgs.dovecot_pigeonhole ]";
        description = ''
          Symlinks the contents of lib/dovecot of every given package into
          the haskell modules directory. This will make the given modules available
          if a dovecot package with the module_dir patch applied is being used.
        '';
      };

      config = mkOption {
        type = with types;
          let
            self = attrsOf (nullOr (oneOf [
              str
              int
              package
              bool
              (listOf (oneOf [ str int package bool ]))
              (attrsOf self)
            ]));
          in
          self // { description = "loop breaker"; };
        description = "Dovecot configuration entries in Nix format.";
        default = { };
        apply = x: pkgs.writeText "dovecot.conf" (nglib.generators.toDovecot x);
      };

      extConfig = mkOption {
        type = with types;
          let
            self = attrsOf (nullOr (oneOf [
              str
              int
              package
              bool
              (listOf (oneOf [ str int package bool ]))
              (attrsOf self)
            ]));
          in
          self // { description = "loop breaker"; };
        description = "Extra config files to generate, if you pass in a config attrset, you can access the generated file via the `config.services.dovecot.extConfig.<name>` attribute.";
        default = { };
        apply = x: mapAttrs (n: v: pkgs.writeText n (nglib.generators.toDovecot v)) x;
      };
    };
  };

  config = mkIf cfg.enable
    {
      users.users."dovecot" = mkIf (cfg.user == "dovecot") {
        uid = ids.uids.dovecot;
        description = "Dovecot user.";
        group = "dovecot";
      };
      users.groups."dovecot" = mkIf (cfg.group == "dovecot") {
        gid = ids.gids.dovecot;
      };
      users.users."vmail" = mkIf (cfg.mailUser == "vmail") {
        uid = ids.uids.vmail;
        description = "vmail user.";
        group = "vmail";
      };
      users.groups."vmail" = mkIf (cfg.mailGroup == "vmail") {
        gid = ids.gids.vmail;
      };
      users.users."dovenull" = mkIf (cfg.loginUser == "dovenull") {
        uid = ids.uids.dovenull;
        description = "Dovecot untrusted login user.";
        group = "dovenull";
      };
      users.groups."dovenull" = mkIf (cfg.loginUser == "dovenull") {
        gid = ids.gids.dovenull;
      };

      environment.systemPackages = with pkgs; [ cfg.package ];

      services.dovecot = {
        config = {
          default_login_user = mkIf (cfg.loginUser != null) cfg.loginUser;
          default_internal_user = mkIf (cfg.user != null) cfg.user;
          default_internal_group = mkIf (cfg.group != null) cfg.group;

          auth_mechanisms = mkDefault "plain";

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

      init.services.dovecot =
        {
          ensureSomething.link."modules" = mkDefault {
            src = modulesDir;
            dst = "/etc/dovecot/modules";
            persistent = false;
          };

          ensureSomething.link."config" = mkDefault {
            src = cfg.config;
            dst = "/etc/dovecot/dovecot.conf";
            persistent = false;
          };

          script = pkgs.writeShellScript "dovecot-run"
            ''
              echo ${cfg.package}/sbin/dovecot -F
              ${cfg.package}/sbin/dovecot -F
            '';
          enabled = true;
        };
    };
}
