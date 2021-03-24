{ buildLinux
, linux
, pkg-config
, ncurses
, writeText
}:
let
  configfile = builtins.toString (writeText "config" ''
    CONFIG_EFI=y
    CONFIG_EFI_STUB=y

    CONFIG_TTY=y

    CONFIG_64BIT=y
    CONFIG_X86_64=y

    CONFIG_PRINTK=y
    CONFIG_PROC_FS=y

    CONFIG_TTY=y
    # CONFIG_VT=y
    # CONFIG_VT_CONSOLE=y
    # CONFIG_UNIX98_PTYS=y 
    # CONFIG_LEGACY_PTYS=y
    # CONFIG_LEGACY_PTY_COUNT=256
    # CONFIG_LDISC_AUTOLOAD=y

    CONFIG_EMBEDDED=y
    CONFIG_PCSPKR_PLATFORM=y
    CONFIG_PRINTK=y
    
    CONFIG_BLK_DEV_INITRD=y
    # CONFIG_RD_GZIP=y
    # CONFIG_RD_BZIP2=y
    # CONFIG_RD_LZMA=y
    # CONFIG_RD_XZ=y
    # CONFIG_RD_LZO=y
    # CONFIG_RD_LZ4=y

    CONFIG_BINFMT_ELF=y

    CONFIG_PROC_FS=y
    # CONFIG_PROC_SYSCTL=y
    CONFIG_PROC_PAGE_MONITOR=n 

    KEXEC=y
  '');

  origKernel = buildLinux {
    inherit (linux) src version stdenv;
    inherit configfile;

    kernelPatches = [];
    allowImportFromDerivation = true;
  };
in
origKernel.overrideAttrs (old: {
  isModular = (builtins.trace old.buildFlags false);

  buildFlags = [
    "KBUILD_BUILD_VERSION=1-NixNG"
    "bzImage"
    "vmlinux"
  ];
  configurePhase = ''
      runHook preConfigure

      mkdir build
      export buildRoot="$(pwd)/build"

      echo "manual-config configurePhase buildRoot=$buildRoot pwd=$PWD"

      runHook postConfigure

      # make $makeFlags "''${makeFlagsArray[@]}" mrproper

      make $makeFlags "''${makeFlagsArray[@]}" tinyconfig
      cat ${configfile} >> $buildRoot/.config
      echo $buildFlags

  #     make $makeFlags "''${makeFlagsArray[@]}" prepare

      # Note: https://github.com/NixOS/nixpkgs/blob/9c213398b312e0f0bb9cdf05090fd20223a82ad0/pkgs/os-specific/linux/kernel/manual-config.nix#L166
      buildFlagsArray+=("KBUILD_BUILD_TIMESTAMP=$(date -u -d @$SOURCE_DATE_EPOCH)")      
      
      ls build
    '';

  # nativeBuildInputs = old.nativeBuildInputs ++ [ pkg-config ncurses ];
})
