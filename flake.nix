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
              makeInitramfs = callPackage ./lib/make-initramfs.nix {};
              makeBundle = callPackage ./lib/make-bundle.nix;
              makeSystem = callPackage ./lib/make-system.nix;
              makeBootloader = callPackage ./lib/make-bootloader;
              runInVm = callPackage ./lib/vm/run-in-vm.nix;
              writeSubstitutedShellScript = callPackage ./lib/write-substituted-shell-script.nix {};
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
            bootloader = {
              enable = true;
              initrdCompression = [ "gzip" ];
            };
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

        dockerTest = ((self.lib "x86_64-linux").makeSystem {
          system = "x86_64-linux";
          name = "nixng-docker";
          config = ({ pkgs, ... }: {
            runit.enable = true;
            services.getty.tty = {
              baudRate = 38400;
            };
            services.apache2 = {
              enable = true;
              premade.basic = {
                enable = true;
              };
              configuration = {
                LoadModule = [
                  [ "mpm_event_module" "modules/mod_mpm_event.so" ]
                  [ "log_config_module" "modules/mod_log_config.so" ]
                  [ "unixd_module" "modules/mod_unixd.so" ]
                  [ "authz_core_module" "modules/mod_authz_core.so" ]
                  [ "dir_module" "modules/mod_dir.so" ]
                  [ "mime_module" "modules/mod_mime.so" ]
                ];

                Listen = "0.0.0.0:80";

                ServerRoot = "/var/www";
                ServerName = "blowhole";
                PidFile = "/httpd.pid";

                User = "www-data";
                Group = "www-data";

                DocumentRoot = "/var/www";

                AddType = [
                  [ "image/svg+xml" "svg" "svgz" ]
                ];
                AddEncoding = [ "gzip" "svgz" ];

                TypesConfig = "\${TYPES_CONFIG}";

                Directory = {
                  "/" = {
                    Require = [ "all" "denied" ];
                    Options = "SymlinksIfOwnerMatch";
                  };
                };

                VirtualHost = {
                  "*:80" = {
                    Directory = {
                      "/var/www" = {
                        Require = [ "all" "granted" ];
                        Options = [ "-Indexes" "+FollowSymlinks" ];
                        DirectoryIndex = "index.html";
                      };
                    };
                  }; 
                };
              };
            };
          });
        });

        overlay = import ./overlay;
        # packages = nixpkgs.lib.genAttrs
        #   supportedSystems
        #   (s: import nixpkgs { system = s; overlays = [ self.overlay ]; });
      };
}
