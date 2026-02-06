# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  lib,
  config,
  system,
  ...
}:
let
  cfg = config.nixpkgs;
  inherit (lib)
    mkOptionType
    mkOption
    types
    singleton
    foldr
    ;

  overlayType = mkOptionType {
    name = "nixpkgs-overlay";
    description = "nixpkgs overlay";
    check = lib.isFunction;
    merge = lib.mergeOneOption;
  };

  upstreamOpts =
    (import "${cfg.pkgs.path}/nixos/modules/misc/nixpkgs.nix" {
      inherit lib;
      options = { };
      config = { };
      pkgs = { };
    }).options.nixpkgs;
in
{
  options.nixpkgs = {
    pkgs = mkOption {
      description = ''
        Packages to use as global `pkgs`;
      '';
      type = types.unspecified;
    };

    config = upstreamOpts.config;

    overlays = mkOption {
      description = ''
        Nixpkgs overlays to apply to global `pkgs`;
      '';
      default = [ ];
      type = types.listOf overlayType;
    };
  };

  config = {
    nixpkgs.overlays = singleton (import ../overlay);
    _module.args.pkgs = import cfg.pkgs.path {
      inherit system;
      overlays = config.nixpkgs.overlays;
      config = config.nixpkgs.config;
    };
  };
}
