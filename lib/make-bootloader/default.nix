{ system, pkgs
, writeTextFile
, runCommandNoCC
, bootloaderLinux
, nglib

, name ? "bootloader"
, kernelExtraConfig ? {}
}:
let
  init = runCommandNoCC "${name}-init"
    { nativeBuildInputs = [ pkgs.busybox ];
      inherit (pkgs) busybox bash;
    }
    ''
      mkdir -p $out

      substituteAll ${pkgs.writeShellScript "init" (builtins.readFile ./init.sh)} $out/init
      chmod +x $out/init
    '';
  linux = bootloaderLinux {
    extraConfig = kernelExtraConfig;
    initramfs = nglib.makeInitramfs {
      name = "initramfs.cpio";
      path = nglib.makeBundle { path = init; name = "init"; };
      compress = false;
    };
  };
in
runCommandNoCC name (with pkgs; {
  inherit busybox;
  nativeBuildInputs = [ pkgs.busybox ];
})
  ''
    cp ${linux}/bzImage $out
  ''
