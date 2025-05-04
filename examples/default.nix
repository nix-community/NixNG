# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  nglib,
  nixpkgs,
  nixng,
}:
let
  modifiedMakeSystem = { config ? {}, ... }@args: nglib.makeSystem ( args //{
    config = {
      nixos.acceptRisks = "I accept the risks";

      imports = [
        config
      ];
    };
  });

  modifiedNglib = nglib // {
    makeSystem = modifiedMakeSystem;
  };

  examples = {
    "gitea" = ./gitea;
    "gitea-sane" = ./gitea/sane.nix;
    "apache" = ./apache;
    "nginx" = ./nginx;
    "nginx-nixos" = ./nginx-nixos;
    "crond" = ./crond;
    "nix" = ./nix;
    "hydra" = ./hydra;
    "certbot" = ./certbot;
    "postfix" = ./postfix;
    "pantalaimon" = ./pantalaimon;
    "jmusicbot" = ./jmusicbot;
    "php-fpm" = ./php-fpm;
    "home-assistant" = ./home-assistant;
    "syncthing" = ./syncthing;
    "initrd" = ./initrd;
    "dnsmasq" = ./dnsmasq;
    "attic" = ./attic;
    "ntfy-sh" = ./ntfy-sh;
    "environment-etc" = ./environment-etc;
    "file-hammer" = ./file-hammer;
  };
in
nixpkgs.lib.mapAttrs (_: v: import v { inherit nixpkgs nixng; nglib = modifiedNglib; }) examples
