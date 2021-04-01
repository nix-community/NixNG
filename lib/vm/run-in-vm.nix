{ pkgs, lib, callPackage
, bash, busybox
, runCommandNoCC
, runVmLinux
, nglib
, writeTextFile, writeText, writeShellScript

, storeDir ? builtins.storeDir
, qemu ? pkgs.qemu
, qemuMem ? 512
, script
, postProcess ? writeShellScript "post-process.sh"
  ''
    cp -r $xchg/out/* $out   
  ''
, preProcess ? null
}:
# TODO NixOS really complicated this, I'd love to know why
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/vm/default.nix
let
  inherit (callPackage (import ./qemu-flags.nix) {}) qemuBinary qemuSerialDevice;
  initrd = nglib.makeInitramfs
    { name = "initrd.img";
      path = nglib.makeBundle
        { name = "init";
          path = writeTextFile
            { name = "init";
              # Heavily inspired by NixOS
              text =
                ''
                #!${bash}/bin/bash

                export PATH=${busybox}/bin:${bash}/bin

                
                mkdir -p /proc /sys
                mount -t proc none /proc
                mount -t sysfs none /sys
                
                source /proc/cmdline  
                
                echo 2 > /proc/sys/vm/panic_on_oom 

                mount -t devtmpfs devtmpfs /dev

                mkdir -p /dev/shm /dev/pts
                mount -t tmpfs -o "mode=1777" none /dev/shm
                mount -t devpts none /dev/pts

                mkdir -p /tmp
                mount -t tmpfs none /tmp

                echo "mounting Nix store..."
                mkdir -p ${storeDir} /host-store
                mount -t 9p store /host-store -o trans=virtio,version=9p2000.L,cache=loose
                mount -t overlay overlay -o lowerdir=/host-store:${storeDir} ${storeDir}
                echo "mounting xchg..."
                xchg="/xchg"
                mkdir -p $xchg
                mount -t 9p xchg $xchg -o trans=virtio,version=9p2000.L
                
                mkdir -p /etc
                ln -sf /proc/mounts /etc/mtab
                echo "127.0.0.1 localhost" > /etc/hosts
                # Ensures tools requiring /etc/passwd will work (e.g. nix)
                if [ ! -e /etc/passwd ]; then
                  echo "root:x:0:0:System administrator:/root:/bin/sh" > /etc/passwd
                fi
                
                out=/xchg/out $script
                echo $? > /xchg/exit-code
              '';
              executable = true;
              destination = "/init";
            };
        };
    };
in
runCommandNoCC "qemu"
  {
    requiredSystemFeatures = [ "kvm" ];
    passAsFile = []; # HACK fix - see https://github.com/NixOS/nixpkgs/issues/16742
  }
# lib.overrideDerivation (writeText "qemu"
  # TODO add rng driver to the kernel
  ''
    mkdir -p $out

    if [ "$enableParallelBuilding" = 1 ]; then
      if [ ''${NIX_BUILD_CORES:-0} = 0 ]; then
        QEMU_OPTS+=" -smp cpus=$(nproc)"
      else
        QEMU_OPTS+=" -smp cpus=$NIX_BUILD_CORES"
      fi
    fi

    _xchg=$(mktemp -d)
    _out=$_xchg/out
    mkdir $_out

    ${lib.optionalString (preProcess != null) ''
      (
        export xchg=$_xchg
        if ! ${preProcess} ; then
          exit 1
        fi
      )
    ''}

    ${qemuBinary qemu} \
      -virtfs local,path=${storeDir},security_model=none,mount_tag=store \
      -virtfs local,path=$_xchg,security_model=none,mount_tag=xchg \
      -device virtio-rng-pci \
      -nographic -no-reboot \
      -m ${toString qemuMem} \
      -kernel ${runVmLinux}/bzImage \
      -initrd ${initrd} \
      -append "panic=1 script=${script} console=${qemuSerialDevice}"
    
    (
      export xchg=$_xchg
      if ! ${postProcess} ; then
        exit 1
      fi
    )
                
    exit $(<$_xchg/exit-code)
  ''
# ({ ... }: {
#   requiredSystemFeatures = [ "kvm" ];
#   passAsFile = []; # HACK fix - see https://github.com/NixOS/nixpkgs/issues/16742
# })
