{ lib, ... }:
{
  options.mrsk = {
    targetHost = lib.mkOption {
      type = lib.types.str;
    };

    remoteUser = lib.mkOption {
      type = lib.types.str;
    };
  };
}
