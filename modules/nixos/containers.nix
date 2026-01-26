{ nglib }:
{
  pkgs,
  lib,
  config,
  ...
}:
let
  hostHostName = config.networking.hostName;
  hostDomain = config.networking.domain;
  specialArgs = config._module.specialArgs;
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

                options = {
                  nixpkgs = lib.mkOption {
                    type = lib.types.nullOr lib.types.unspecified;
                    default = null;
                    description = ''
                      Uninstantiated nixpkgs to use as the main package set for this NixNG
                      configuration. If set to `null`, the host's instantiated nixpkgs instance
                      will be re-used instead. Re-using a already instantiated `nixpkgs` instance,
                      speeds up evaluation.
                    '';
                  };

                  name = lib.mkOption {
                    type = lib.types.str;
                    default = name;
                    description = ''
                      The name of this NixNG configuration.
                    '';
                  };

                  system = lib.mkOption {
                    type = lib.types.nullOr lib.types.str;
                    default = pkgs.stdenv.hostPlatform.system;
                    defaultText = "pkgs.stdenv.hostPlatform.system";
                    description = ''
                      The system for which this NixNG configuration should be built for, only has
                      an effect, if `nixpkgs` is non-null.
                    '';
                  };

                  config = lib.mkOption {
                    type = lib.types.deferredModule;
                    description = ''
                      The user configuration for this NixNG instance.
                    '';
                  };

                  defaultModules = lib.mkOption {
                    type = lib.types.listOf lib.types.deferredModule;
                    default = import ../list.nix;
                    defaultText = "import ../list.nix";
                    description = ''
                      Set of default modules for this NixNG configuration, generally shouldn't be
                      changed.
                    '';
                  };

                  specialArgs = lib.mkOption {
                    type = lib.types.attrsOf lib.types.unspecified;
                    defaultText = "lib.mapAttrs (lib.const lib.mkDefault) specialArgs";
                    description = ''
                      Set of `specialArgs` for this NixNG configuration, defaults to the hosts
                      `specialArgs`.
                    '';
                  };
                };

                config = {
                  specialArgs = lib.mapAttrs (lib.const lib.mkDefault) specialArgs;
                };
              }
            );
            default = null;

            description = ''
              Parameters for a NixNG configuration. This is then passed to `nglib.makeSystem`
              to actually instantiate NixNG.
            '';

            apply =
              x:
              let
                evaled = nglib.makeSystem (
                  x
                  // {
                    config = {
                      _file = "${__curPos.file}:${toString __curPos.line}";
                      config = {
                        nixpkgs.pkgs = lib.mkIf (x.nixpkgs != null) (lib.mkForce pkgs);
                        networking.domain = "${hostHostName}.${hostDomain}";
                      };
                      imports = [
                        x.config
                      ];
                    };
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
