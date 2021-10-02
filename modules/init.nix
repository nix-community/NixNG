/*
 * NixNG
 * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>   
 *  
 *  This file is free software: you may copy, redistribute and/or modify it  
 *  under the terms of the GNU General Public License as published by the  
 *  Free Software Foundation, either version 3 of the License, or (at your  
 *  option) any later version.  
 *  
 *  This file is distributed in the hope that it will be useful, but  
 *  WITHOUT ANY WARRANTY; without even the implied warranty of  
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
 *  General Public License for more details.  
 *  
 *  You should have received a copy of the GNU General Public License  
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.  
 */

{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.init;

  log = mkOption {
    description = "Logging settings.";
    type = with types; nullOr (submodule ({ config, ... }:
      let
        cfg = config;
      in
        {
          options = {
            file = mkOption {
              description = "Log to a plain file, without rotation.";
              type = nullOr (submodule {
                options = {
                  dst = mkOption {
                    description = "The file to which to log to.";
                    type = path;
                  };
                  rotate = mkOption {
                    description = "The size after which the file should rotated in kilo bytes.";                  
                    type = int;
                    default = 0; # 1 MB
                  };
                };
              });
              default = null;
            };
            syslog = mkOption {
              description = "Log via syslog, either to a UDS or over TCP/UDP.";
              type = nullOr (submodule ({ config, ... }:
                let
                  cfg = config;
                in {
                  options = {
                    type = mkOption {
                      description = "Syslog type, UDS, TCP, or UDP.";
                      type = enum [ "uds" "tcp" "udp" ];
                    };
                    dst = mkOption {
                      description = "The endpoint to log to, format depends on the type.";
                      type = str;
                    };
                    time = mkOption {
                      description = "Whether the complete sender timestamp should be included in log messages";
                      type = bool;
                      default = false;
                    };
                    host = mkOption {
                      description = "Whether the hostname should be included in log messages.";
                      type = bool;
                      default = true;
                    };
                    timeQuality = mkOption {
                      description = "Whether time quality information should be included in the log messages.";
                      type = bool;
                      default = false;
                    };
                    tag = mkOption {
                      description = "Every message will be marked with this tag.";
                      type = nullOr str;
                      default = null;
                    };
                    priority = mkOption {
                      description = "Mark every message with a priority.";
                      type = nullOr str;
                      default = null;
                    };
                  };

                  config = {
                    dst =
                      if cfg.type == "uds" then
                        mkDefault "/dev/log"
                      else if cfg.type == "tcp" || cfg.type == "udp" then
                        mkDefault "127.0.0.1:514"
                      else
                        abort "Unknown syslog type, this should have been caught by the module system!";
                  };
                }));
              default = null;
            };
          };
          config = {};
        }));
    default = {};
  };
  ensureSomething = mkOption {
    description = "Files or directories which need to exist before service is started, no overwriting though.";
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
    shutdown = mkOption {
      description = "A script which successfully shuts down the system.";
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

          inherit ensureSomething log;

          shutdownOnExit = mkEnableOption "Whether the init system should enter shutdown when this particular service exits";

          script = mkOption {
            description = "Service script to start the program.";
            type = types.path;
            default = "";
          };

          finish = mkOption {
            description = "Script to run upon service stop.";
            type = with types; nullOr path;
            default = null;
          };

          enabled = mkOption {
            description = "Whether the service should run on startup.";
            type = types.bool;
            default = false;
          };

          environment = mkOption {
            description = ''
              The environment variables that will be added to the default ones prior to running <option>script</option>
              and <option>finish</option>.
            '';
            type = with types; attrsOf (oneOf [ str int ]);
            default = {};
          };

          pwd = mkOption {
            description = ''
              Directory in which both <option>script</option> and <option>finish</option> will be ran.
            '';
            type = types.str;
            default = "/var/empty";
          };
        };
      });
      description = "Service definitions.";
      default = {};
    };
  };

  config = {
    # TODO add assertions for this module
    assertions = mapAttrsToList (n: v:
      [ { assertion =
            let
              selectedCount =
                (count (x: x) (mapAttrsToList (n: v: if v == null then false else true) v.log));
            in
              selectedCount == 1 || selectedCount == 0;
          message = "You can only select one log type, in service ${n}.";
        }
        {
          assertion = (v.log.file.rotate.rotate or 0) >= 0;
          message = "init.service.<name>.log.file.rotate can't be less than 0";
        }
      ]
    ) cfg.services;
  };
}
