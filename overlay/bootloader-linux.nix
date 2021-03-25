# TODO use linuxManualConfig instead of buildLinux

{ buildLinux
, linux
, pkg-config
, ncurses
, writeText
, runCommand
}:
let
  readConfig = configfile: import (runCommand "config.nix" {} ''
    echo "{" > "$out"
    while IFS='=' read key val; do
      [ "x''${key#CONFIG_}" != "x$key" ] || continue
      no_firstquote="''${val#\"}";
      echo '  "'"$key"'" = "'"''${no_firstquote%\"}"'";' >> "$out"
    done < "${configfile}"
    echo "}" >> $out
  '').outPath;
  configfile = builtins.toString (writeText "config" ''
    # EFI boot and EFI stub
    CONFIG_EFI=y
    CONFIG_EFI_STUB=y

    # Enable 64-bit
    CONFIG_64BIT=y
    CONFIG_X86_64=y

    # TTY stuff
    CONFIG_TTY=y
    CONFIG_VT=y
    CONFIG_VT_CONSOLE=y
    CONFIG_UNIX98_PTYS=y 
    CONFIG_LEGACY_PTYS=y
    CONFIG_LEGACY_PTY_COUNT=256
    CONFIG_LDISC_AUTOLOAD=y

    # Serial console, makes it work in QEMU
    CONFIG_SERIAL_8250=y
    CONFIG_SERIAL_8250_CONSOLE=y

    # Enables some advanced options, like the printk one
    CONFIG_EMBEDDED=y
    # Enable PC speaker, beep boop
    CONFIG_PCSPKR_PLATFORM=y
    # Debug logs during boot are always handy
    CONFIG_PRINTK=y
    
    # Initrd things, perhaps make the compression configurable?
    CONFIG_BLK_DEV_INITRD=y
    CONFIG_RD_GZIP=y
    CONFIG_RD_BZIP2=y
    CONFIG_RD_LZMA=y
    CONFIG_RD_XZ=y
    CONFIG_RD_LZO=y
    CONFIG_RD_LZ4=y

    # Required for shebangs
    CONFIG_BINFMT_ELF=y # general execution
    CONFIG_BINFMT_SCRIPT=y # bash shebang

    ## Filesystems
    # proc
    CONFIG_PROC_FS=y
    CONFIG_PROC_SYSCTL=y
    CONFIG_PROC_PAGE_MONITOR=n 
    # sys
    CONFIG_SYSFS=y
    # configfs - TODO do we need it?
    CONFIG_CONFIGFS_FS=y
    # devtmpfs
    CONFIG_DEVTMPFS=y

    ## Networking
    CONFIG_NET=y # NET
    # Unix Domain Sockets
    CONFIG_UNIX=y # udevd

    # inotify
    CONFIG_INOTIFY_USER=y # udevd

    CONFIG_KEXEC=y
  '');

  origKernel = buildLinux {
    inherit (linux) src version stdenv;
    inherit configfile;
    config = readConfig configfile;

    kernelPatches = [];
    allowImportFromDerivation = true;
  };
self = 
origKernel.overrideAttrs (old: {
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
      if [[ -f $buildRoot/.config ]] ; then
        cat ${configfile} >> $buildRoot/.config
        cat $buildRoot/.config
      else
        echo "$buildRoot/.config is empty, not appending"
        exit 1
      fi
      echo $buildFlags

      # make $makeFlags "''${makeFlagsArray[@]}" prepare

      # Note: https://github.com/NixOS/nixpkgs/blob/9c213398b312e0f0bb9cdf05090fd20223a82ad0/pkgs/os-specific/linux/kernel/manual-config.nix#L166
      buildFlagsArray+=("KBUILD_BUILD_TIMESTAMP=$(date -u -d @$SOURCE_DATE_EPOCH)")      
      
      ls build
    '';

  postInstall = "";

  nativeBuildInputs = old.nativeBuildInputs ++ [ pkg-config ncurses ];
});
in
self
