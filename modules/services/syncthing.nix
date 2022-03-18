# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, nglib, ... }:
with nglib; with lib;
let
  cfg = config.services.syncthing;

  dataDir = "/var/syncthing";
in
{
  options.services.syncthing = {
    enable = mkEnableOption "Enable SyncThing";

    package = mkOption {
      description = "syncthing package to use.";
      default = pkgs.syncthing;
      type = types.package;
    };

    config = mkOption {
      description = ''
        Configuration options for syncthing.
      '';
      type = format.type;
      default = {};
    };

    user = mkOption {
      description = "Syncthing user.";
      type = types.str;
      default = "syncthing";
    };

    group = mkOption {
      description = "Syncthing group.";
      type = types.str;
      default = "syncthing";
    };

    guiAddress = mkOption {
      description = "Syncthing GUI address";
      type = types.str;
      default = "http://127.0.0.1:8384/";
    };
  };


  config = {
    init.services.syncthing = {
      enabled = true;
      script = pkgs.writeShellScript "syncthing-run"
        ''
          mkdir -p ${dataDir}/{.,storage,data,config}

          chown -R ${cfg.user}:${cfg.group} ${dataDir}
          chmod -R u=rwX,g=r-X,o= ${dataDir}

          export PATH=$PATH:${cfg.package}/bin \
                 HOME=${dataDir}/storage
          chpst -u ${cfg.user}:${cfg.group} -b syncthing syncthing serve \
            --no-browser \
            --gui-address=${cfg.guiAddress} \
            --data=${dataDir}/data \
            --config=${dataDir}/config
        '';
    };

    users.users.${cfg.user} = mkDefaultRec {
      description = "Syncthing";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.syncthing;
    };

    users.groups.${cfg.group} = mkDefaultRec {
      gid = config.ids.gids.syncthing;
    };
  }; 
}
