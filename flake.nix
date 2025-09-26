# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    buildbot-nix.url = "github:nix-community/buildbot-nix";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      nixpkgs,
      self,
      treefmt-nix,
      ...
    }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "i686-linux"
        "aarch64-linux"
      ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;
      pkgsForSystem =
        system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

      treefmtEval = forAllSystems (
        system: treefmt-nix.lib.evalModule (pkgsForSystem system) ./treefmt.nix
      );

      inherit (nixpkgs.lib) recurseIntoAttrs;
    in
    {
      nglib = import ./lib {
        inherit (nixpkgs) lib;
        inherit inputs;
      };
      examples = import ./examples {
        inherit nixpkgs;
        inherit (self) nglib;
        nixng = self;
      };
      overlays.default = import ./overlay;

      legacyPackages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import ./overlay) ];
          };
          lib = pkgs.lib;
        in
        lib.genAttrs (lib.attrNames (import ./overlay null null)) (packageName: pkgs.${packageName})
      );

      packages = forAllSystems (
        system:
        let
          lib = nixpkgs.lib;
        in
        lib.filterAttrs (_: v: lib.isDerivation v) self.legacyPackages.${system}
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsForSystem system;
        in
        {
          default = pkgs.mkShell { nativeBuildInputs = with pkgs; [ ]; };
        }
      );

      checks =
        {
          formatting = forAllSystems (system: treefmtEval.${system}.config.build.check self);
          examples = recurseIntoAttrs (
            nixpkgs.lib.mapAttrs (n: v: v.config.system.build.toplevel) self.examples
          );
        }
        // forAllSystems (
          system:
          let
            pkgs = pkgsForSystem system;
          in
          {
            with-lib = pkgs.callPackage ./checks/with-lib.nix { inherit self; } {
              allowed = [
                "/doc/style_and_vocabulary.org"
                "/checks"
              ];
            };
          }
        );

      formatter = forAllSystems (system: (treefmtEval.${system}.config.build.wrapper));
    };
}
