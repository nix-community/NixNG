{
  lib,
  nglib,
  config,
  pkgs,
  ...
}:
{
  options = {
    nixos.systemd.services = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              description = lib.mkOption {
                type = lib.types.str;
                default = name;
              };

              after = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };

              wants = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };

              wantedBy = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };

              requires = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };

              requiredBy = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };

              environment = lib.mkOption {
                type = lib.types.attrsOf lib.types.str;
                default = { };
              };

              path = lib.mkOption {
                type = lib.types.listOf lib.types.package;
                default = { };
              };

              preStart = lib.mkOption {
                type = lib.types.lines;
                default = "";
              };

              reloadIfChanged = lib.mkOption { type = lib.types.bool; };

              serviceConfig = lib.mkOption {
                type = lib.types.submodule {
                  options = {
                    Type = lib.mkOption { type = lib.types.str; };

                    ExecStart = lib.mkOption {
                      type = lib.types.either (lib.types.listOf lib.types.str) lib.types.str;
                      apply = x: lib.toList x;
                    };

                    ExecReload = lib.mkOption {
                      type = lib.types.either (lib.types.listOf lib.types.str) lib.types.str;
                      apply = x: lib.toList x;
                    };

                    User = lib.mkOption { type = lib.types.str; };

                    Group = lib.mkOption { type = lib.types.str; };

                    WorkingDirectory = lib.mkOption { type = lib.types.path; };

                    LoadCredential = lib.mkOption {
                      type = lib.types.listOf lib.types.str;
                      default = [ ];
                    };

                    RuntimeDirectory = lib.mkOption { type = lib.types.str; };

                    Environment = lib.mkOption {
                      type = lib.types.listOf lib.types.str;
                      default = [ ];
                    };

                    OOMPolicy = lib.mkOption {
                      type = lib.types.str;
                      default = "";
                    };
                  };
                };
              };
            };
          }
        )
      );
      default = { };
    };

    nixos.systemd.tmpfiles = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = {
    init.services = (
      lib.flip lib.mapAttrs config.nixos.systemd.services (
        n: v:
        let
          combinedDeps = lib.unique (v.after ++ v.wants ++ v.requires);
          withCredentials = v.serviceConfig.LoadCredential != [ ];
          credentialsDirectory = "/run/credentials/${n}";
        in
        {
          dependencies = lib.flip lib.map combinedDeps (
            e:
            lib.pipe e [
              (lib.strings.removeSuffix ".service")
              (lib.strings.removeSuffix ".target")
            ]
          );
          enabled = lib.elem "multi-user.target" (v.wantedBy ++ v.requiredBy);
          environment = lib.mkMerge [
            v.environment
            {
              PATH = lib.makeBinPath v.path;
              CREDENTIALS_DIRECTORY = lib.mkIf withCredentials credentialsDirectory;
            }
            (lib.listToAttrs (
              lib.map (
                env:
                let
                  parts = lib.splitString "=" env;
                  envKey = lib.elemAt parts 0;
                  envValue = lib.replaceStrings [ "%d" ] [ credentialsDirectory ] (lib.elemAt parts 1);
                in
                assert (lib.assertMsg (lib.length parts == 2) "\"${env}\" is not of the format \"KEY=VALUE\"");
                lib.nameValuePair envKey envValue
              ) v.serviceConfig.Environment
            ))
          ];
          execStartPre = pkgs.writeShellScript "${n}-start-pre" ''
            ${nglib.mergeShellFragmentsIsolated (
              lib.map (x: {
                name = x;
                data = ''
                  umask 077
                  set -euo pipefail
                  _source="$(cut -f 2 -d ':'<<<"${x}")"
                  _dest="${credentialsDirectory}/$(cut -f 1 -d ':'<<<"${x}")"

                  if ! [ -e "$_source" ] ; then
                    printf "Credential $_source for service ${n} not found.\n"
                    exit 1
                  fi

                  cp "$_source" "$_dest"
                  chown ${v.serviceConfig.User}:${v.serviceConfig.Group} "$_dest"
                '';
              }) v.serviceConfig.LoadCredential
            )}

            if ! [ "$_status" = "0" ] ; then
              exit "$_status"
            fi

            ${v.preStart}
          '';
          execStart = pkgs.writeShellScript "${n}-start" (
            lib.concatStringsSep "\n" v.serviceConfig.ExecStart
          );
          group = v.serviceConfig.Group;
          user = v.serviceConfig.User;
          workingDirectory = v.serviceConfig.WorkingDirectory;
          tmpfiles =
            with nglib.nottmpfiles.dsl;
            lib.optionals withCredentials [
              (d credentialsDirectory "0700" config.init.services.${n}.user config.init.services.${n}.group _ _)
              (R credentialsDirectory "0700" config.init.services.${n}.user config.init.services.${n}.group _ _)
            ];
        }
      )
    );
  };
}
