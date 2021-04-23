{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.nix;
in
{
  options.nix = {
    enable = mkEnableOption "Enable Nix, add Nix the global path and creates the necessary folder structure.";

    package = mkOption {
      description = ''
        Which package to use for running Nix related commands, will also be added
        to the global system PATH, TODO.
      '';
      type = types.package;
      default = pkgs.nix;
    };

    buildUserCount = mkOption {
      description = ''
        How many build users, and groups should be created, if Nix runs out,
        increase this number.
      '';
      type = types.int;
      default = 32;
    };

    nixPath = mkOption {
      description = ''
        The Nix Path, basically channels.
      '';
      type = with types; listOf str;
      default = [
        "nixpkgs=${pkgs.path}"
      ];
    };

    readOnlyStore = mkOption {
      description = ''
        Whether the store should be made read-only by using a bind mount.
      '';
      type = types.bool;
      default = true;
      apply = x:
        if x then
          ''
            export PATH=${pkgs.busybox}/bin:${pkgs.utillinux}/bin 

            # Borrowed from NixOS:
            # https://github.com/NixOS/nixpkgs/blob/08546595671398eb7939eb5865d111351c546cf1/nixos/modules/system/boot/stage-2-init.sh

            # Make /nix/store a read-only bind mount to enforce immutability of
            # the Nix store.  Note that we can't use "chown root:nixbld" here
            # because users/groups might not exist yet.
            # Silence chown/chmod to fail gracefully on a readonly filesystem
            # like squashfs.
            chown -f 0:30000 /nix/store
            chmod -f 1775 /nix/store
            if [ -n "@readOnlyStore@" ]; then
              if ! [[ "$(findmnt --noheadings --output OPTIONS /nix/store)" =~ ro(,|$) ]]; then
                # FIXME when linux < 4.5 is EOL, switch to atomic bind mounts
                #mount /nix/store /nix/store -o bind,remount,ro
                mount --bind /nix/store /nix/store
                mount -o remount,ro,bind /nix/store
              fi
            fi
          ''
        else
          "true";
    };
  };

  config = mkIf cfg.enable {
    system.createNixRegistration = true;
    system.activation.loadNixDb = 
      (nglib.dag.dagEntryAfter [ "currentSystem" "users" ]
        ''
          export PATH=${pkgs.busybox}/bin:${cfg.package}/bin

          if [[ ! -d /nix/var/nix/db ]] ; then
            nix-store --init
            nix-store --load-db < /run/current-system/registration
          fi
        '');

    users = {
      users = mkMerge (map (x:
        {
          "nixbld${toString x}" = {
            uid = 30000 + x;
            group = "nixbld";
            home = "/var/empty";
            createHome = false;
            description = "Nix build user ${toString x}";
            shell = "${pkgs.busybox}/bin/nologin";
          };
        })
        (range 0 cfg.buildUserCount));

      groups.nixbld.gid = 30000;
    };

    environment.variables = {
      NIX_PATH = cfg.nixPath;
    };
  };
}
 
