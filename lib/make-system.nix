{ pkgs, system
, runCommandNoCC, lib
, busybox, bootloaderLinux
, config, name
, nglib
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
              
          ${if activation.enable then
            "ln -s ${activation.script} $out/activation"
           else ""}
          ln -s ${init.script} $out/init
          ${if initramfs.enable then
              "ln -s ${initramfs.image} $out/initrd.img"
            else ""}
          ${if bootloader.enable then
            "ln -s ${bootloaderLinux} $out/bootloader"
            else ""}
        '');
  systemBundle = nglib.makeBundle
    { name = "system";
      path = systemPath;
    };
  initramfsImage = nglib.makeInitramfs 
    { name = "initrd.img";
      path = systemBundle;
    };
  qemu = {
    run = pkgs.writeShellScript "qemu-run.sh" ''
      ${pkgs.qemu}/bin/qemu-system-x86_64 -kernel ${systemPath}/bootloader/bzImage -nographic -append "console=ttyS0" -initrd ${systemPath}/initrd.img -m 512 
    ''; # /run/current-system/kernel
  };
in 
{
  system = systemPath;
  bundle = systemBundle;
  initramfs = initramfsImage;
  inherit qemu;
}
