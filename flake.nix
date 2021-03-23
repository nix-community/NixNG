{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { nixpkgs, self }:
    let
      defaultModules = [
        ./modules/runit
        ./modules/initrd
        ./modules/initramfs
        ./modules/init.nix
        ./modules/activation
        ./modules/system.nix
        ./modules/assertions.nix
      ];

      buildSystem =
        { system,
          config,
        }:

        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          evaledModules = pkgs.lib.evalModules
            { 
              modules = defaultModules ++ [ config ({ ... }:
                {
                  _module.args = {
                    inherit pkgs system;
                    nixng = self;
                  };
                }
              )];
            };
          systemClosure = pkgs.runCommandNoCC "system"
            { nativeBuildInputs = [
                pkgs.busybox
              ];
            }
            (let
              failedAssertions = builtins.map (x: x.message) (builtins.filter (x: !x.assertion) evaledModules.config.assertions);
              config =
                if failedAssertions != [] then
                      throw "\nFailed assertions:\n${lib.concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
                else
                  evaledModules.config;
            in ''
              mkdir $out
              
              
              ln -s ${config.activation.script} $out/activation
              ln -s ${config.init.script} $out/init
              ${if config.initramfs.enable then
                  "ln -s ${config.initramfs.image} $out/initrd.img"
                else ""}
            '');
          bundle =
            let
              systemReferences = pkgs.writeReferencesToFile systemClosure;
            in
              pkgs.runCommandNoCC "system"
                {}
                ''
                  shopt -s dotglob

                  mkdir $out
                  cp -r ${systemClosure}/* $out

                  for path in $(cat ${systemReferences}); do
                    test -f $path && ( mkdir -p $out/$(dirname $path) ; cp -r $path $out/$path )
                    test -d $path && ( mkdir -p $out/$path ; cp -r $path/* $out/$path )
                  done
                '';
          initramfs = pkgs.runCommandNoCC "initramfs.img"
            { nativeBuildInputs = with pkgs; [
                findutils
                cpio
                gzip
              ];
            }
            ''
              ( cd ${bundle} ; find . | cpio -o -H newc --quiet | gzip -9 ) > $out
            '';
          qemu = {
            run = pkgs.writeShellScript "qemu-run.sh" ''
              ${pkgs.qemu}/bin/qemu-system-x86_64 -kernel /run/current-system/kernel -nographic -append "console=ttyS0" -initrd ${systemClosure}/initrd.img -m 512 
            '';
          };
        in
          {
            system = systemClosure;
            inherit bundle initramfs qemu;
          };
    in
      {
        lib = {
          inherit buildSystem;
        };

        testSystem = buildSystem {
          system = "x86_64-linux";
          config = ({ pkgs, ... }: {
            system.environment.files = [
              {
                source = "${pkgs.hello}/bin/hello";
                destination = "/bin/hello";
              }
            ];

            runit.enable = true;
            initramfs = {
              enable = true; 
              config = {
                system.environment.files = [
                  {
                    source = "${pkgs.vim}/bin/vim";
                    destination = "/bin/vim";
                  }
                ];
                initrd.enable = true;
                activation.enable = true;
              }; 
            };
            initrd.enable = false;
          });
        };
      };
}
