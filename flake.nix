{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { nixpkgs, self }:
    let
      defaultModules = [
        ./modules/runit
        ./modules/initrd
        ./modules/init.nix
        ./modules/system.nix
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
                    inherit pkgs;
                  };
                }
              )];
            };
          activationScript = import ./activation.nix
            { inherit pkgs;
              config = evaledModules.config;
            };
          systemClosure = pkgs.runCommandNoCC "system"
            { nativeBuildInputs = [
                pkgs.busybox
              ];
            }
            ''
              mkdir $out
              
              ln -s ${activationScript} $out/activation
              ln -s ${evaledModules.config.init.script} $out/init
            '';
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
                    test -f $path && cp -r $path $out/$path
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
              find ${bundle}
              ( cd ${bundle} ; find . | cpio -o -H newc --quiet | gzip -9 ) > $out
            '';
        in
          {
            system = systemClosure;
            inherit bundle initramfs;
          };
    in
      {

        testSystem = buildSystem {
          system = "x86_64-linux";
          config = ({ pkgs, ... }: {
            system.environment.files = [
              {
                source = "${pkgs.hello}/bin/hello";
                destination = "/bin/hello";
              }
            ];

            runit.enable = false;
            initrd.enable = true;
          });
        };
      };
}
