# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, config, ... }:
let
  cfg = config.nixpkgs;
  inherit (lib)
    mkOptionType
    mkOption
    types
    singleton
    ;
  overlayType =
    mkOptionType {
      name = "nixpkgs-overlay";
      description = "nixpkgs overlay";
      check = lib.isFunction;
      merge = lib.mergeOneOption;
    };
in
{
  options.nixpkgs = {
    pkgs = mkOption {
      description = ''
        Packages to use as global `pkgs`;
      '';
      type = types.unspecified;
    };

    overlays = mkOption {
      description = ''
        Nixpkgs overlays to apply to global `pkgs`;
      '';
      default = [];
      type = types.listOf overlayType;
    };
  };

  config = {
    nixpkgs.overlays = singleton (import ../overlay);
    _module.args.pkgs =
      cfg.pkgs.appendOverlays config.nixpkgs.overlays;
  };
}
