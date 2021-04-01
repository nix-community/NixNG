{ callPackage
, lib
, tinyLinux
}:
{ initramfs
, extraConfig ? {}
}:
tinyLinux.override {
  extraConfig = {
    # EFI boot and EFI stub
    CONFIG_EFI="y";
    CONFIG_EFI_STUB="y";

    # Enable 64-bit
    CONFIG_64BIT="y";
    CONFIG_X86_64="y";

    # TTY stuff
    CONFIG_TTY="y";
    CONFIG_VT="y";
    CONFIG_VT_CONSOLE="y";
    CONFIG_UNIX98_PTYS="y"; 
    CONFIG_LEGACY_PTYS="y";
    CONFIG_LEGACY_PTY_COUNT="256";
    CONFIG_LDISC_AUTOLOAD="y";

    # Serial console, makes it work in QEMU
    CONFIG_SERIAL_8250="y";
    CONFIG_SERIAL_8250_CONSOLE="y";

    # Enables some advanced options, like the printk one
    CONFIG_EMBEDDED="y";
    # Enable PC speaker, beep boop
    CONFIG_PCSPKR_PLATFORM="y";
    # Debug logs during boot are always handy
    CONFIG_PRINTK="y";
    
    CONFIG_RD_GZIP="y";
    # Initrd things, perhaps make the compression configurable?
    CONFIG_BLK_DEV_INITRD="y";
    CONFIG_INITRAMFS_SOURCE=''"${initramfs}"'';
    # CONFIG_INITRAMFS_COMPRESSION="gzip";
    # CONFIG_INITRAMFS_COMPRESSION_GZIP="y";

    # Required for shebangs
    CONFIG_BINFMT_ELF="y"; # general execution
    CONFIG_BINFMT_SCRIPT="y"; # bash shebang

    ## Filesystems
    # proc
    CONFIG_PROC_FS="y";
    CONFIG_PROC_SYSCTL="y";
    CONFIG_PROC_PAGE_MONITOR="n";
    # sys
    CONFIG_SYSFS="y";
    # configfs - TODO do we need it?
    CONFIG_CONFIGFS_FS="y";
    # devtmpfs
    CONFIG_DEVTMPFS="y";

    ## Networking
    CONFIG_NET="y"; # NET
    # Unix Domain Sockets
    CONFIG_UNIX="y"; # udevd

    # inotify
    CONFIG_INOTIFY_USER="y"; # udevd

    CONFIG_KEXEC="y";
    
    CONFIG_BLOCK="y";
  } // extraConfig;
}
