{ lib, nglib, ... }:
{
  mkUserOption =
    user: description:
    lib.mkOption {
      inherit description;
      type = lib.types.str;
      default = user;
    };

  mkGroupOption =
    group: description:
    lib.mkOption {
      inherit description;
      type = lib.types.str;
      default = group;
    };
}
