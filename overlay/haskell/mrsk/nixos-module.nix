{ lib, ... }:
{
  options.mrsk = {
    targetHost = lib.mkOption {
      type = lib.types.str;
      description = ''
        The host `mrsk` should deploy to.
      '';
    };

    remoteUser = lib.mkOption {
      type = lib.types.str;
      description = ''
        The user `mrsk` should login as when deploying to `targetHost`.
      '';
    };
  };
}
