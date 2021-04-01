{ nglib, writeShellScript
, busybox, utillinux, dosfstools, e2fsprogs, parted, grub2

, system
}:

assert system.config.bootloader.enable;

nglib.runInVm {
  preProcess = writeShellScript "script"
    ''
      export PATH=${busybox}/bin:${parted}/bin
      set -e

      dd if=/dev/zero of=$xchg/out/image.img bs=1024 count=524288
      
      parted --script $xchg/out/image.img \
        mklabel msdos \
        mkpart primary 1MiB 100% 
    '';
  script = writeShellScript "script"
    ''
      export PATH=${dosfstools}/bin:${e2fsprogs}/bin:${utillinux}/bin:${busybox}/bin:${parted}/bin:${grub2}/bin
      set -e
                  
      _root=$(mktemp -d)
      _loopdev=$(losetup -f)

      losetup -P $_loopdev $out/image.img
      mkfs.ext4 ''${_loopdev}p1

      mount ''${_loopdev}p1 $_root

      cp -r ${system.bundle}/* $_root

      mkdir -p $_root/boot $_root/etc
      ln -sfn /proc/mounts $_root/etc/mtab
      grub-install --root-directory=$_root --boot-directory=$_root/boot --target=i386-pc /dev/loop0 
  '';
}
