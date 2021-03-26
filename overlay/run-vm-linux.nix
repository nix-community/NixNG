{ callPackage
, lib
, tinyLinux
}:
tinyLinux.override {
  extraConfig = {
    CONFIG_NET_9P="y";
    CONFIG_NET_9P_VIRTIO="y";
    CONFIG_NET_9P_DEBUG="y"; # perhaps?
    CONFIG_9P_FS="y";
    CONFIG_9P_FS_POSIX_ACL="y";

    CONFIG_NETWORK_FILESYSTEMS="y";
    CONFIG_INET="y";
    CONFIG_NET="y";
    CONFIG_9P="y";

    CONFIG_VIRTIO="y";

    CONFIG_PCI="y";
    CONFIG_VIRTIO_PCI="y";
    CONFIG_VIRTIO_MENU="y";
    CONFIG_PCI_HOST_GENERIC="y"; # (only needed for the QEMU Arm 'virt' board) from wikipedia

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

    # Debug logs during boot are always handy
    CONFIG_PRINTK="y";
    
    # Initrd things, perhaps make the compression configurable?
    CONFIG_BLK_DEV_INITRD="y";
    CONFIG_RD_GZIP="y";
    CONFIG_RD_BZIP2="n";
    CONFIG_RD_LZMA="n";
    CONFIG_RD_XZ="n";
    CONFIG_RD_LZO="n";
    CONFIG_RD_LZ4="n";

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
    # devtmpfs
    CONFIG_DEVTMPFS="y";
  };
}
