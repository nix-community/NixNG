module:
{ pkgs, lib, config, options, ... }:
let
  inherit (lib)
    types
    mkOption
    setAttrByPath
    getAttrFromPath
    mkOptionType
    concatStringsSep
    attrNames
    foldl'
    mkMerge
    mkIf
    elem
    concatStrings
    mkDefault
    mkEnableOption
    optional
    concatMapStringsSep
    ;

  rootConfig = config;
  rootOptions = options;

  cfg = getAttrFromPath [ "services" "gitea" ] rootConfig;

  # A type that is one of several submodules, similiar to types.oneOf but is usable inside attrsOf or listOf
  # submodules need an option with a type str which is used to find the corresponding type
  taggedSubmodules =
    { types
    , specialArgs ? {}
    }: mkOptionType rec {
    name = "taggedSubmodules";
    description = "one of ${concatStringsSep "," (attrNames types)}";
    check = x: if x ? type then types.${x.type}.check x else throw "No type option set in:\n${lib.generators.toPretty {} x}";
    merge = loc: foldl'
      (res: def: types.${def.value.type}.merge loc [
        (lib.recursiveUpdate { value._module.args = specialArgs; } def)
      ])
      { };
    nestedTypes = types;
  };
in
(lib.evalModules {
  modules = [
    module
    ({ config, ... }: {
      options = {
        output = mkOption {
          type = types.unspecified;
          internal = true;
        };

        user = mkOption {
          type = types.str;
          default = "forgejo";
        };

        repositoryRoot = mkOption {
          type = types.str;
          default = "${config.stateDirectory}/repositories";
        };

        lfs = {
          enable = mkEnableOption "Enable LFS";

          contentDirectory = mkOption {
            type = types.str;
            default = "${config.stateDirectory}/data/lfs";
          };
        };

        database = mkOption {
          type = taggedSubmodules {
            types.mysql = types.submodule {
              options = {
                type = mkOption {
                  type = types.enum ["mysql"];
                };

                name = mkOption {
                  type = with types; str;
                };

                user = mkOption {
                  type = with types; str;
                };

                host = mkOption {
                  type = with types; nullOr str;
                  default = null;
                };

                port = mkOption {
                  type = with types; nullOr port;
                  default = null;
                };

                socket = mkOption {
                  type = with types; nullOr str;
                  default = null;
                };
              };
            };
            types.postgres = types.submodule {
              options = {
                type = mkOption {
                  type = types.enum ["postgres"];
                };

                name = mkOption {
                  type = with types; str;
                };

                user = mkOption {
                  type = with types; str;
                };

                host = mkOption {
                  type = with types; nullOr str;
                  default = null;
                };

                port = mkOption {
                  type = with types; nullOr port;
                  default = null;
                };

                socket = mkOption {
                  type = with types; nullOr str;
                  default = null;
                };
              };
            };

            types.sqlite3 = types.submodule {
              options = {
                type = mkOption {
                  type = types.enum ["sqlite3"];
                };

                path = mkOption {
                  type = types.str;
                };
              };
            };
          };
        };

        stateDirectory = mkOption {
          type = types.str;
          default = "/var/lib/gitea";
        };
      };

      config.output = {
        assertions =
          let
            remoteDatabase = elem config.database.type [ "postgresql" "mysql" ];
            requiredSecrets = [
              "lfsJwtSecret"
              "internalToken"
              "jwtSecret"
              "secretKey"
            ] ++ (optional remoteDatabase "databasePassword");
          in [
            {
              assertion = lib.foldl (a: b: a && (cfg.secrets ? ${b})) true [
              ];
              message = ''
                The Gitea sane requires the following secrets:
              '' + concatMapStringsSep "\n" (secret: "  - ${secret}") requiredSecrets;
            }
            {
              assertion = with config.database;
                remoteDatabase ->
                (host == null && socket != null && port == null) ||
                (host != null && socket == null && port != null);
              message = concatStrings [
                "Setting the PostgreSQL database by `database.host`, "
                "`database.port` is exclusive with `database.sockets`"
                ", but at least one must be set."
              ];
            }
          ];
        init.services.gitea.environment = {
          GITEA_WORK_DIR = config.stateDirectory;
        };
        services.gitea = {
          settings = {
            default = {
              RUN_MODE = "prod";
              RUN_USER = config.user;
              WORK_PATH = config.stateDirectory;
            };

            server = {
              LFS_START_SERVER = config.lfs.enable;
              LFS_JWT_SECRET = "@${cfg.secrets.lfsJwtSecret.placeholder}@";
              APP_DATA_PATH = config.stateDirectory;
            };

            database = mkMerge [
              {
                DB_TYPE = config.database.type;
              }
              (mkIf (elem config.database.type [ "postgres" "mysql" ]) {
                HOST =
                  if config.database.socket != null then
                    config.database.socket
                  else
                    config.database.host + ":" + toString config.database.port;
                NAME = config.database.name;
                USER = config.database.user;
                PASSWD = "@${cfg.secrets.databasePassword.placeholder}@";
              })
              (mkIf (config.database.type == "sqlite") {
                PATH = config.database.path;
              })
              (mkIf (config.database.type == "postgres") {
                SSL_MODE = "disable";
              })
            ];

            repository = {
              ROOT = config.repositoryRoot;
            };

            session = {
              COOKIE_NAME = mkDefault "session";
            };

            security = {
              SECRET_KEY = "@${cfg.secrets.secretKey.placeholder}@";
              INTERNAL_TOKEN = "@${cfg.secrets.internalToken.placeholder}@";
              INSTALL_LOCK = true;
            };

            # mailer = mkIf (config.mailerPasswordFile != null) {
            #   PASSWD = "#mailerpass#";
            # };

            oauth2 = {
              JWT_SECRET = "@${cfg.secrets.jwtSecret.placeholder}@";
            };

            lfs = mkIf config.lfs.enable {
              PATH = config.lfs.contentDirectory;
            };
          };
        };
      };
    })
  ];
  prefix = [ "prefabs" "gitea" ];
}).config.output
