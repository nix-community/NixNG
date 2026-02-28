# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      nixpkgs,
      self,
      treefmt-nix,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "i686-linux"
        "aarch64-linux"
      ];
      forAllSystems' = lib.genAttrs;
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

      inherit (nixpkgs) lib;
    in
    {
      nglib = import ./lib nixpkgs.lib;
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
        system: lib.filterAttrs (_: v: lib.isDerivation v) self.legacyPackages.${system}
      );

      nixosModules = {
        nixngContainers = lib.modules.importApply ./modules/nixos/containers.nix { inherit (self) nglib; };
      };

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsForSystem system;
        in
        {
          default = pkgs.mkShell { nativeBuildInputs = with pkgs; [ ]; };
          haskell = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              ghc
              cabal-install
              cabal2nix
              haskell-language-server
              haskellPackages.implicit-hie
              fourmolu
              jq
              zlib
            ];
          };
        }
      );

      checks = {
        formatting = forAllSystems (system: treefmtEval.${system}.config.build.check self);
        examples = lib.recurseIntoAttrs (lib.mapAttrs (n: v: v.config.system.build.toplevel) self.examples);
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

      nixosModules = {
        mrsk = ./overlay/haskell/mrsk/nixos-module.nix;
      };

      formatter = forAllSystems (system: (treefmtEval.${system}.config.build.wrapper));
    };
}
