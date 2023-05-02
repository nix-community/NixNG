# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, config, nglib, ... }:
with lib; with nglib;
let
  cfg = config.environment;
in
{
  options.environment = {
    variables = mkOption {
      default = { };
      example = { EDITOR = "vim"; BROWSER = "firefox"; };
      type = with types; attrsOf (either str (listOf str));
      apply = mkApply
        (x: concatStringsSep ":"
          (mapAttrsToList (n: v: "${n}=" + (if isList v then concatStringsSep ":" v else v)) x));
    };

    shell = {
      enable = mkOption {
        description = ''
          Enable the packages needed to get a shell.
        '';
        type = types.bool;
        default = true;
      };
      profile = mkOption {
        description = ''
          Shell script fragments, concataned into /etc/profile.
        '';
        type = with types; listOf str;
        apply = mkApply
          (x: pkgs.writeShellScript "profile" (concatStringsSep "\n" x));
        default = [ ];
      };
    };

    createBaseEnv = mkOption {
      description = ''
        Create /bin/sh, /usr/bin/env, /var/empty, and /tmp.
      '';
      type = types.bool;
      default = true;
    };

    systemPackages = mkOption {
      description = "Packages globally added to PATH";
      default = [ ];
      type = with types; listOf package;
    };
  };

  config = {
    environment.systemPackages = with pkgs; [ runit busybox ];

    environment.shell.profile = [
      ''
        ${optionalString (cfg.variables.applied != "") "export ${cfg.variables.applied}"}
        export PATH="$PATH"':${makeBinPath cfg.systemPackages}'
      ''
    ];

    system.activation.shellProfile = nglib.dag.dagEntryAnywhere ''
      export PATH=${pkgs.busybox}/bin

      mkdir -m 0755 -p /etc
      ln -sfn ${cfg.shell.profile.applied} /etc/.profile.tmp
      mv /etc/.profile.tmp /etc/profile # atomically replace /etc/profile
    '';

    system.activation.createBaseEnv = mkIf cfg.createBaseEnv
      (nglib.dag.dagEntryAnywhere
        ''
          export PATH=${pkgs.busybox}/bin

          # Borrowed from NixOS therefore it's licensed under the MIT license
          #### Activation script snippet usrbinenv:
          _localstatus=0
          mkdir -m 0755 -p /usr/bin
          ln -sfn ${pkgs.busybox}/bin/env /usr/bin/.env.tmp
          mv /usr/bin/.env.tmp /usr/bin/env # atomically replace /usr/bin/env

          # Create the required /bin/sh symlink; otherwise lots of things
          # (notably the system() syscall) won't work.
          mkdir -m 0755 -p /bin
          ln -sfn "${pkgs.busybox}/bin/sh" /bin/.sh.tmp
          mv /bin/.sh.tmp /bin/sh # atomically replace /bin/sh

          mkdir -pm 0777 /tmp
          mkdir -pm 0555 /var/empty
        '');
  };
}
