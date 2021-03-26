{ pkgs, lib, callPackage
, bash, busybox
, runCommandNoCC
, runVmLinux
, nglib
, writeTextFile, writeText

, storeDir ? builtins.storeDir
, qemu ? pkgs.qemu
, qemuMem ? 512
, script
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

                echo "mounting Nix store..."
                mkdir -p ${storeDir}
                mount -t 9p store ${storeDir} -o trans=virtio,version=9p2000.L,cache=loose
                sleep 5

                echo "mounting out..."
                mkdir -p /out
                mount -t 9p out /out -o trans=virtio,version=9p2000.L
                
                mkdir -p /etc
                ln -sf /proc/mounts /etc/mtab
                echo "127.0.0.1 localhost" > /etc/hosts
                # Ensures tools requiring /etc/passwd will work (e.g. nix)
                if [ ! -e /etc/passwd ]; then
                  echo "root:x:0:0:System administrator:/root:/bin/sh" > /etc/passwd
                fi
                
                $script
                
                # poweroff -f # that works, the kernel tries to reboot after /init exits but qemu doesnt honor the request and exits
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

    ${qemuBinary qemu} \
      -virtfs local,path=${storeDir},security_model=none,mount_tag=store \
      -virtfs local,path=$out,security_model=none,mount_tag=out \
      -device virtio-rng-pci \
      -nographic -no-reboot \
      -m ${toString qemuMem} \
      -kernel ${runVmLinux}/bzImage \
      -initrd ${initrd} \
      -append "panic=1 script=${script} console=${qemuSerialDevice}"
  ''
# ({ ... }: {
#   requiredSystemFeatures = [ "kvm" ];
#   passAsFile = []; # HACK fix - see https://github.com/NixOS/nixpkgs/issues/16742
# })
