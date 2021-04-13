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
            };

        giteaSystem = import ./examples/gitea self.lib;
        apacheSystem = import ./examples/apache self.lib;
        apacheRunitSystem = import ./examples/apache-runit self.lib;
        crondSystem = import ./examples/crond self.lib;

        overlay = import ./overlay;
        packages = nixpkgs.lib.genAttrs
          supportedSystems
          (s: import nixpkgs { system = s; overlays = [ self.overlay ]; });
      };
}
