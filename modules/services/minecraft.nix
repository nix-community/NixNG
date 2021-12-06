# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.minecraft;
  format = pkgs.formats.json {};

  jqMeld = pkgs.writeText "meld.jq"
    ''
      # Recursively meld a and b,
      # concatenating arrays and
      # favoring b when there is a conflict
      def meld(a; b):
        a as $a | b as $b
        | if ($a|type) == "object" and ($b|type) == "object"
          then reduce ([$a,$b]|add|keys_unsorted[]) as $k ({};
            .[$k] = meld( $a[$k]; $b[$k]) )
          elif ($a|type) == "array" and ($b|type) == "array"
          then $a+$b
          elif $b == null then $a
          else $b
          end;
    '';
  forgeConfigured = pkgs.runCommandNoCC "forge-configured" {}
    ''
      set -xe

      mkdir -p $out
      cd $out
      ${pkgs.unzip}/bin/unzip ${cfg.forgeZipFile}

      _server_folder="$out/"*"/"
      mv $_server_folder/* .
      rm -r $_server_folder

      ${pkgs.yq-go}/bin/yq eval-all '. as $item ireduce ({}; . *+ $item)' server-setup-config.yaml ${cfg.forgeConfigOverrides} > server-setup-config-new.yaml
      mv server-setup-config-new.yaml server-setup-config.yaml
    '';

  forgeFetched = pkgs.stdenv.mkDerivation {
    name = "forge";
    src = forgeConfigured;

    nativeBuildInputs = with pkgs; [
      # fakeJava
      cfg.javaPackage
      curl
      cacert
    ];

    buildPhase = ''
      sh startserver.sh
    '';

    installPhase = ''
      mkdir -p $out
      cp -r * $out
    '';

    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = cfg.forgeFetchedHash;
  };
in
{
  options.services.minecraft = {
    enable = mkEnableOption "Enable Minecraft server service.";

    forgeZipFile = mkOption {
      type = with types; nullOr path;
      description = ''
        Path to zip file containing a Forge formatted MC server.
      '';
      default = null;
    };

    forgeConfigOverrides = mkOption {
      type = with types; format.type;
      description = ''
        Config overrides to apply to Forge config file.
      '';
      default = {};
      apply = x: pkgs.writeText "forge-config-overrides.json" (builtins.toJSON x);
    };

    forgeFetchedHash = mkOption {
      type = with types; str;
      description = ''
        The output hash of running `serverstart.sh`.
      '';
    };

    javaPackage = mkOption {
      type = with types; package;
      description = ''
        Which Java path should be used.
      '';
      default = pkgs.jdk11;
    };

    overlayfsPackage = mkOption {
      type = with types; package;
      description = ''
        Which Java path should be used.
      '';
      default = pkgs.fuse-overlayfs;
    };

    eulaAccept = mkOption {
      type = with types; bool;
      description = ''
        By changing the setting below to true you are indicating your agreement to Mojang's EULA (https://account.mojang.com/documents/minecraft_eula).
      '';
      default = false;
    };
  };

  config = mkIf cfg.enable {
    init.services.minecraft =
      {
        script = pkgs.writeShellScript "minecraft-run"
          ''
            set -xe

            mkdir -p /run/cfg/minecraft/{upperdir,workdir}
            ${cfg.overlayfsPackage}/bin/fuse-overlayfs -o lowerdir=${forgeFetched},upperdir=/run/cfg/minecraft/upperdir,workdir=/run/cfg/minecraft/workdir /run/cfg/minecraft

            cd /run/cfg/minecraft
            echo "eula=true" > ./eula.txt
            export PATH=$PATH:${cfg.javaPackage}/bin
            sh ./startserver.sh
          '';
        enabled = true;
      };

    assertions = [
      { assertion = cfg.eulaAccept;
        message = "You must accept the EULA";
      }
    ];
  };
}
