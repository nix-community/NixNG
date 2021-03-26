{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { nixpkgs, self }:
    let
      supportedSystems = [ "x86_64-linux" "i386-linux" "aarch64-linux" ];
      systemed = system: rec {
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        callPackage = pkgs.lib.callPackageWith ({
          nglib = self.lib system;
          inherit pkgs;
        } // pkgs);
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
              makeInitramfs = callPackage ./lib/make-initramfs.nix;
              makeBundle = callPackage ./lib/make-bundle.nix;
              makeSystem = callPackage ./lib/make-system.nix;
            };

        testSystem = (self.lib "x86_64-linux").makeSystem {
          system = "x86_64-linux";
          name = "nixng-system";
          config = ({ pkgs, ... }: {
            system.environment.files = [
              {
                source = "${pkgs.hello}/bin/hello";
                destination = "/bin/hello";
              }
            ];

            runit.enable = true;
            bootloader.enable = true;
            initramfs = {
              enable = true; 
              config = {
                # system.environment.files = [
                #   {
                #     source = "${pkgs.vim}/bin/vim";
                #     destination = "/bin/vim";
                #   }
                # ];
                initrd.enable = true;
              }; 
            };
          });
        };

        vmTest = (self.lib "x86_64-linux").runInVm {
          script = (systemed "x86_64-linux").pkgs.writeShellScript "script"
            ''
              echo "asdasd" > /out/file.txt
            '';
        };

        overlay = import ./overlay;
        packages = nixpkgs.lib.genAttrs
          supportedSystems
          (s: import nixpkgs { system = s; overlays = [ self.overlay ]; });
      };
}
