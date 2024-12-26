{ lib, config, ... }:
{
  options = {
    nixos.users.users = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            description = lib.mkOption { type = lib.types.str; };
            createHome = lib.mkOption { type = lib.types.bool; };
            home = lib.mkOption { type = lib.types.path; };
            group = lib.mkOption { type = lib.types.str; };
            extraGroups = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
            };
            useDefaultShell = lib.mkOption { type = lib.types.bool; };
            isSystemUser = lib.mkOption {
              type = lib.types.bool;
              default = true;
            };
            isNormalUser = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
          };
        }
      );
      default = { };
    };

    nixos.users.groups = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule { });
      default = { };
    };

  };

  config = {
    users.users =
      (lib.mapAttrs (
        _: v: lib.filterAttrs (n: _: !lib.elem n [ "isSystemUser" ]) v
      ) config.nixos.users.users);
    users.groups = config.nixos.users.groups;
  };
}
