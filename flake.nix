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

{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { nixpkgs, self }:
    let
      supportedSystems = [ "x86_64-linux" "i386-linux" "aarch64-linux" ];
      systemed = system: rec {
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        callPackage = pkgs.lib.callPackageWith (pkgs // {
          nglib = self.lib system;
          pkgs = pkgs // { inherit callPackage; };
          callPackage = callPackage;
        });
        lib = pkgs.lib;
        nglib = self.lib system;
      };
    in
      {
        lib = system:
          let
            inherit (systemed system) callPackage;
          in
            {
              makeSystem = callPackage ./lib/make-system.nix;
              runInVm = callPackage ./lib/vm/run-in-vm.nix;
              writeSubstitutedShellScript = callPackage ./lib/write-substituted-shell-script.nix {};
              writeSubstitutedFile = callPackage ./lib/write-substituted-file.nix {};
              writeSubstitutedShellScriptBin = callPackage ./lib/write-substituted-shell-script-bin.nix {};
              dag = callPackage ./lib/dag.nix {};
              generators = callPackage ./lib/generators.nix {};
            };

        giteaSystem = import ./examples/gitea self.lib;
        apacheSystem = import ./examples/apache self.lib;
        apacheRunitSystem = import ./examples/apache-runit self.lib;
        crondSystem = import ./examples/crond self.lib;
        nixSystem = import ./examples/nix self.lib;
        hydraSystem = import ./examples/hydra self.lib;

        overlay = import ./overlay;
        packages = nixpkgs.lib.genAttrs
          supportedSystems
          (s: import nixpkgs { system = s; overlays = [ self.overlay ]; });
      };
}
