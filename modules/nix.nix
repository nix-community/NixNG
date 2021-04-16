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

    buildUserCound = mkOption {
      description = ''
        How many build users, and groups should be created, if Nix runs out,
        increase this number.
      '';
      type = types.int;
      default = 32;
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
            description = "Nix build user ${toString x}";
            shell = "${pkgs.busybox}/bin/nologin";
          };
        })
        [ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
          22 23 24 25 26 27 28 29 30 31 32 ]);

      groups.nixbld.gid = 30000;
    };
  };
}
 
