{ pkgs, callPackage, system, dockerTools
, runCommandNoCC, lib, nglib
, busybox, bootloaderLinux
, config, name
}:

let
  defaultModules = [
    ../modules/runit
    ../modules/initrd
    ../modules/initramfs
    ../modules/init.nix
    ../modules/activation
    ../modules/system.nix
    ../modules/assertions.nix
    ../modules/bootloader
    ../modules/services/apache2.nix
    ../modules/services/getty.nix
  ];

  evaledModules = lib.evalModules
    { 
      modules = defaultModules ++ [ config ({ ... }:
        {
          _module.args = {
            inherit pkgs system nglib;
          };
        }
      )];
    };

  failedAssertions = map (x: x.message) (lib.filter (x: !x.assertion) evaledModules.config.assertions);
  configValid =
    if failedAssertions != [] then
      throw "\nFailed assertions:\n${lib.concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
    else
      evaledModules.config;
  systemPath = 
    runCommandNoCC name
      { nativeBuildInputs = [
          busybox
        ];
      }
      (with configValid;
        ''
          mkdir $out
          ln -s ${init.script} $out/init
        '');

          # mkdir $out
              
          # ${lib.optionalString activation.enable
          #   "# ln -s ${activation.script} $out/activation"}
          # ${lib.optionalString initramfs.enable
          #   "ln -s ${initramfs.image} $out/initrd.img"}
          # ${lib.optionalString bootloader.enable
          #   "ln -s ${nglib.makeBootloader { inherit (bootloader) kernelExtraConfig; }} $out/bootloader"}
  systemBundle = nglib.makeBundle
    { name = "${name}-bundle";
      path = systemPath;
    };
  initramfsImage = nglib.makeInitramfs 
    { name = "${name}-initrd.img";
      path = systemBundle;
    };
  qemu = {
    run = pkgs.writeShellScript "qemu-run.sh" ''
      ${pkgs.qemu}/bin/qemu-system-x86_64 -kernel ${systemPath}/bootloader -initrd ${systemPath}/initrd.img -nographic -append "console=ttyS0" -m 512 
    ''; # /run/current-system/kernel
  };

  self = {
    system = systemPath;
    bundle = systemBundle;
    initramfs = initramfsImage;
    config = evaledModules.config;
    iso = callPackage ./make-iso-image.nix { system = self; };
    inherit qemu;
    ociImage = dockerTools.buildLayeredImage {
      inherit name;
      tag = "latest";

      contents = [ systemBundle ];

      config = {
        Entrypoint =
          [ "/init"
          ];
      };
    };
  };
in 
self
