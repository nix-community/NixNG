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

{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.environment;
  profileScript = pkgs.writeShellScript "profile" cfg.shell.profile;
in
{
  options.environment = {
    variables = mkOption {
      default = {};
      example = { EDITOR = "vim"; BROWSER = "firefox"; };
      type = with types; attrsOf (either str (listOf str));
      apply = x: concatStringsSep ":"
        (mapAttrsToList (n: v: "${n}=" + (if isList v then concatStringsSep ":" v else v)) x);
    };

    shell = {
      profile = mkOption {
        description = ''
          Shell script fragments, concataned into /etc/profile.
        '';
        type = with types; listOf str;
        apply = x: concatStringsSep "\n" x;
        default = [];
      };
    };

    createBaseEnv = mkOption {
      description = ''
        Create /bin/sh, /usr/bin/env, /var/empty, and /tmp.
      '';
      type = types.bool;
      default = true;
    };

  };

  config = {
    environment.shell.profile =[
      ''
        export ${cfg.variables}
      ''
    ];

    system.activation.shellProfile = nglib.dag.dagEntryAnywhere ''
      export PATH=${pkgs.busybox}/bin

      mkdir -m 0755 -p /etc
      ln -sfn ${profileScript} /etc/.profile.tmp 
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
          # (notably the system() function) won't work.
          mkdir -m 0755 -p /bin
          ln -sfn "${pkgs.busybox}/bin/sh" /bin/.sh.tmp
          mv /bin/.sh.tmp /bin/sh # atomically replace /bin/sh

          mkdir -pm 0777 /tmp
          mkdir -pm 0555 /var/empty
        '');
  };
}
