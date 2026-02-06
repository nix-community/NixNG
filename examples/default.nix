# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  nglib,
  nixpkgs,
  nixng,
}:
let
  examples = {
    "gitea" = ./gitea;
    "gitea-sane" = ./gitea/sane.nix;
    "apache" = ./apache;
    "nginx" = ./nginx;
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
nixpkgs.lib.mapAttrs (_: v: import v { inherit nixpkgs nglib nixng; }) examples
