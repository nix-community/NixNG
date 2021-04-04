{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.init;
in
{
  options.init = {
    type = mkOption {
      description = "Selected init system.";
      type = types.enum cfg.type;
    };
    availableInits = mkOption {
      description = "List of available init systems.";
      type = types.listOf types.str;
    };
    script = mkOption {
      description = "init script.";
      type = types.path;
    };
    services = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          dependencies = mkOption {
            description = "Service dependencies";
            type = types.listOf (types.str);
            default = [];
          };

          ensureSomething = mkOption {
            description = "Files or directories which need to exist before service is started, no overwriting though";
            default = {};
            type = with types;
              submodule {
                options = {
                  link = mkOption {
                    type = attrsOf (submodule {
                      options = {
                        src = mkOption {
                          description = "The source of the link.";
                          type = path;
                        };
                        dst = mkOption {
                          description = "The destination of the link.";
                          type = path;
                        };
                        persistent = mkOption {
                          description = "Whether the created something should be kept after service stop.";
                          type = bool;
                          default = false;
                        };
                      };
                    }); 
                    default = {};
                  };
                  copy = mkOption {
                    type = attrsOf (submodule {
                      options = {
                        src = mkOption {
                          description = "The source of the copy.";
                          type = path;
                        };
                        dst = mkOption {
                          description = "The destination of copy.";
                          type = path;
                        };
                        persistent = mkOption {
                          description = "Whether the created something should be kept after service stop.";
                          type = bool;
                          default = false;
                        };
                        # TODO add mode?
                      };
                    }); 
                    default = {};
                  };
                  linkFarm = mkOption {
                    type = attrsOf (submodule {
                      options = {
                        src = mkOption {
                          description = "The source of the link farm.";
                          type = path;
                        };
                        dst = mkOption {
                          description = "The destination of the link farm.";
                          type = path;
                        };
                        persistent = mkOption {
                          description = "Whether the created something should be kept after service stop.";
                          type = bool;
                          default = false;
                        };
                      };
                    }); 
                    default = {};
                  };
                  exec = mkOption {
                    description = "Execute file to create a file or directory.";
                    type = attrsOf (submodule {
                      options = {
                        dst = mkOption {
                          description = "Where should the executable output and what file or folder to check whether it should be run.";
                          type = path;
                        };
                        executable = mkOption {
                          description = "The path to the executable to execute. Use $out.";
                          type = path;
                        };
                        persistent = mkOption {
                          description = "Whether the created something should be kept after service stop.";
                          type = bool;
                          default = false;
                        };
                      };
                    }); 
                    default = {};
                  };
                  create = mkOption {
                    description = "Creates either an empty file or directory.";
                    type = attrsOf (submodule {
                      options = {
                        type = mkOption {
                          description = "Whether to create a direcotroy or file.";
                          type = enum [ "directory" "file" ];
                        };
                        mode = mkOption {
                          description = "Mode to set for the new creation, if set to `null`, its up to the implementation.";
                          default = null;
                          type = nullOr str;
                        };
                        owner = mkOption {
                          description = "Owner of new creation.";
                          default = "root:root";
                          type = str;
                        };
                        dst = mkOption {
                          description = "Wheret to create it.";
                          type = path;
                        };
                        persistent = mkOption {
                          description = "Whether the created something should be kept after service stop.";
                          type = bool;
                          default = false;
                        };
                      };
                    }); 
                    default = {};
                  };
                };
              };
          };

          script = mkOption {
            description = "Service script to start the program.";
            type = types.path;
            default = "";
          };
        };
      });
      description = "Service definitions.";
      default = {};
    };
  };

  config = {
    # TODO add assertions for this module
    assertions = [
      
    ];
  };
}
