# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { nixpkgs, self }:
    let
      supportedSystems = [ "x86_64-linux" "i386-linux" "aarch64-linux" ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;
      pkgsForSystem = system:
        import nixpkgs { inherit system; overlays = [ self.overlay ]; };
    in
    {
      nglib =
        lib:
        let this =
              { makeSystem = import ./lib/make-system.nix { nglib = this; inherit (self) overlay;  };
                dag = import ./lib/dag.nix { inherit lib; };
                generators = import ./lib/generators.nix { inherit lib; };
              };
        in this;

      examples = import ./examples { inherit nixpkgs; inherit (self) nglib; };

      overlay = import ./overlay;
      packages = forAllSystems pkgsForSystem;

      devShell = forAllSystems (system:
        let pkgs = pkgsForSystem system;
        in
        pkgs.mkShell {
          nativeBuildInputs = with pkgs;
            [
              nixpkgs-fmt
              rnix-lsp
              dhall
              reuse
            ];
        });

      hydraJobs = {
        examples = self.examples;
      };
    };
}
