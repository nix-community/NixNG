{
  nglib,
  lib,
  config,
  ...
}:
{
  options = {
    nixos.users.users = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              description = lib.mkOption {
                type = lib.types.str;
                description = ''
                  The users description;
                '';
              };
              createHome = lib.mkOption {
                type = lib.types.bool;
                description = ''
                  Whether to create the user's home folder.
                '';
              };
              home = lib.mkOption {
                type = lib.types.path;
                description = ''
                  Where should the users home folder be located.
                '';
              };
              group = lib.mkOption {
                type = lib.types.str;
                description = ''
                  The user's primary group.
                '';
              };
              extraGroups = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
                description = ''
                  The user's supplementary groups.
                '';
              };
              useDefaultShell = lib.mkOption {
                type = lib.types.bool;
                description = ''
                  Whether to use the default shell.
                '';
              };
              isSystemUser = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = ''
                  This doesn't do anything on NixNG but is here for eval-time compatibility.
                  Slight difference of behavior is expected.
                '';
              };
              isNormalUser = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = ''
                  Indicates whether is an account for a 'real' user.
                '';
              };
              name = lib.mkOption {
                type = lib.types.str;
                default = name;
                readOnly = true;
              };
            };
          }
        )
      );
      default = { };
      description = ''
        NixOS compatible users module. This module should have the same
        semantics as the upstream module, as such see
        [the upstream options](https://search.nixos.org/options?query=users.users).

        WARNING: this module only implements a rather small subset of the upstream
        NixOS module, but the parts that are implemented should have mostly the same
        semantics.
      '';
    };

    nixos.users.groups = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule { });
      default = { };
      description = ''
        NixOS compatible users module. This module should have the same
        semantics as the upstream module, as such see
        [the upstream options](https://search.nixos.org/options?query=users.users).

        Also see the documentation for NixNG's `users.users`, this compatibility module
        maps almost exactly onto it.

        WARNING: this module only implements a rather small subset of the upstream
        NixOS module, but the parts that are implemented should have mostly the same
        semantics.
      '';
    };
  };

  imports = [
    (nglib.mkOptionsEqual
      [
        "users"
        "users"
      ]
      [
        "nixos"
        "users"
        "users"
      ]
      (
        def:
        lib.mapAttrs (_: user:
          lib.removeAttrs user [
            "name"
            "isSystemUser"
          ]
        ) def
      )
    )
    (nglib.mkOptionsEqual
      [
        "users"
        "groups"
      ]
      [
        "nixos"
        "users"
        "groups"
      ]
      lib.id
    )
  ];
}
