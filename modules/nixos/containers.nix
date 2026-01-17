{ nglib }:
{
  pkgs,
  lib,
  config,
  ...
}:
let
  hostHostName = config.networking.hostName;
in
{
  options.containers = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        {
          config,
          name,
          ...
        }:
        {
          options.nixng = lib.mkOption {
            type = lib.types.nullOr (
              lib.types.submodule {
                freeformType = lib.types.unspecified;
              }
            );
            default = null;

            apply =
              x:
              let
                evaled = nglib.makeSystem (
                  x
                  // {
                    name = config.nixng.name or "${name}.${hostHostName}";
                    config = {
                      _file = "${__curPos.file}:${toString __curPos.line}";
                      config.nixpkgs.pkgs = lib.mkForce pkgs;
                      imports = [
                        x.config
                      ];
                    };

                    system = pkgs.stdenv.hostPlatform.system;
                  }
                );
              in
              if x != null then
                assert lib.lists.intersectLists (lib.attrNames x) (lib.attrNames evaled) == [ "config" ];
                x // evaled
              else
                null;
          };

          config = lib.mkIf (config.nixng != null) {
            extraFlags = [
              "--notify-ready=no"
              "--kill-signal=SIGTERM"
            ];

            path = config.nixng.config.system.build.toplevel;
          };
        }
      )
    );
  };
}
