{ makeInitramfs
, pkgs
, qemu ? pkgs.qemu
, runCommandNoCC
, callPackage 

, script
}:
let
  qemuFlags = callPackage (import ./qemu-flags.nix) {};
in
runCommandNoCC "qemu"
  ''
    ${qemu}/
  ''
