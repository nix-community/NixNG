# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, config, ... }:
let
  cfg = config.init;

  log = lib.mkOption {
    description = "Logging settings.";
    type = lib.types.nullOr (
      lib.types.submodule (
        { config, ... }:
        {
          options = {
            file = lib.mkOption {
              description = "Log to a plain file, without rotation.";
              type = lib.types.nullOr (
                lib.types.submodule {
                  options = {
                    dst = lib.mkOption {
                      description = "The file to which to log to.";
                      type = lib.types.path;
                    };
                    rotate = lib.mkOption {
                      description = "The size after which the file should rotated in kilo bytes.";
                      type = lib.types.int;
                      default = 0; # 1 MB
                    };
                  };
                }
              );
              default = null;
            };
            syslog = lib.mkOption {
              description = "Log via syslog, either to a UDS or over TCP/UDP.";
              type = lib.types.nullOr (
                lib.types.submodule (
                  { config, ... }:
                  let
                    cfg = config;
                  in
                  {
                    options = {
                      type = lib.mkOption {
                        description = "Syslog type, UDS, TCP, or UDP.";
                        type = lib.types.enum [
                          "uds"
                          "tcp"
                          "udp"
                        ];
                      };
                      dst = lib.mkOption {
                        description = "The endpoint to log to, format depends on the type.";
                        type = lib.types.str;
                      };
                      time = lib.mkOption {
                        description = "Whether the complete sender timestamp should be included in log messages";
                        type = lib.types.bool;
                        default = false;
                      };
                      host = lib.mkOption {
                        description = "Whether the hostname should be included in log messages.";
                        type = lib.types.bool;
                        default = true;
                      };
                      timeQuality = lib.mkOption {
                        description = "Whether time quality information should be included in the log messages.";
                        type = lib.types.bool;
                        default = false;
                      };
                      tag = lib.mkOption {
                        description = "Every message will be marked with this tag.";
                        type = with lib.types; nullOr str;
                        default = null;
                      };
                      priority = lib.mkOption {
                        description = "Mark every message with a priority.";
                        type = with lib.types; nullOr str;
                        default = null;
                      };
                    };

                    config = {
                      dst =
                        if cfg.type == "uds" then
                          lib.mkDefault "/dev/log"
                        else if cfg.type == "tcp" || cfg.type == "udp" then
                          lib.mkDefault "127.0.0.1:514"
                        else
                          abort "Unknown syslog type, this should have been caught by the module system!";
                    };
                  }
                )
              );
              default = null;
            };
          };
          config = { };
        }
      )
    );
    default = { };
  };
  ensureSomething = lib.mkOption {
    description = "Files or directories which need to exist before service is started, no overwriting though.";
    default = { };
    type =
      with lib.types;
      submodule {
        options = {
          link = lib.mkOption {
            type = attrsOf (submodule {
              options = {
                src = lib.mkOption {
                  description = "The source of the link.";
                  type = path;
                };
                dst = lib.mkOption {
                  description = "The destination of the link.";
                  type = path;
                };
                persistent = lib.mkOption {
                  description = "Whether the created something should be kept after service stop.";
                  type = bool;
                  default = false;
                };
              };
            });
            default = { };
          };
          copy = lib.mkOption {
            type = attrsOf (submodule {
              options = {
                src = lib.mkOption {
                  description = "The source of the copy.";
                  type = path;
                };
                dst = lib.mkOption {
                  description = "The destination of copy.";
                  type = path;
                };
                persistent = lib.mkOption {
                  description = "Whether the created something should be kept after service stop.";
                  type = bool;
                  default = false;
                };
                # TODO add mode?
              };
            });
            default = { };
          };
          linkFarm = lib.mkOption {
            type = attrsOf (submodule {
              options = {
                src = lib.mkOption {
                  description = "The source of the link farm.";
                  type = path;
                };
                dst = lib.mkOption {
                  description = "The destination of the link farm.";
                  type = path;
                };
                persistent = lib.mkOption {
                  description = "Whether the created something should be kept after service stop.";
                  type = bool;
                  default = false;
                };
              };
            });
            default = { };
          };
          exec = lib.mkOption {
            description = "Execute file to create a file or directory.";
            type = attrsOf (submodule {
              options = {
                dst = lib.mkOption {
                  description = "Where should the executable output and what file or folder to check whether it should be run.";
                  type = path;
                };
                executable = lib.mkOption {
                  description = "The path to the executable to execute. Use $out.";
                  type = path;
                };
                persistent = lib.mkOption {
                  description = "Whether the created something should be kept after service stop.";
                  type = bool;
                  default = false;
                };
              };
            });
            default = { };
          };
          create = lib.mkOption {
            description = "Creates either an empty file or directory.";
            type = attrsOf (submodule {
              options = {
                type = lib.mkOption {
                  description = "Whether to create a direcotroy or file.";
                  type = enum [
                    "directory"
                    "file"
                  ];
                };
                mode = lib.mkOption {
                  description = "Mode to set for the new creation, if set to `null`, its up to the implementation.";
                  default = null;
                  type = nullOr str;
                };
                owner = lib.mkOption {
                  description = "Owner of new creation.";
                  default = "root:root";
                  type = str;
                };
                dst = lib.mkOption {
                  description = "Wheret to create it.";
                  type = path;
                };
                persistent = lib.mkOption {
                  description = "Whether the created something should be kept after service stop.";
                  type = bool;
                  default = false;
                };
              };
            });
            default = { };
          };
        };
      };
  };
in
{
  options.init = {
    type = lib.mkOption {
      description = "Selected init system.";
      type = lib.types.enum cfg.type;
    };
    availableInits = lib.mkOption {
      description = "List of available init systems.";
      type = with lib.types; listOf str;
    };
    script = lib.mkOption {
      description = "init script.";
      type = lib.types.path;
    };
    shutdown = lib.mkOption {
      description = "A script which successfully shuts down the system.";
      type = lib.types.path;
    };
    services = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          imports = [
            (lib.mkRenamedOptionModule [ "script" ] [ "execStart" ])
            (lib.mkRenamedOptionModule [ "finish" ] [ "execStop" ])
            (lib.mkRenamedOptionModule [ "pwd" ] [ "workingDirectory" ])
          ];
          options = {
            dependencies = lib.mkOption {
              description = "Service dependencies";
              type = with lib.types; listOf str;
              default = [ ];
            };

            inherit ensureSomething log;

            shutdownOnExit = lib.mkEnableOption "Whether the init system should enter shutdown when this particular service exits";

            execStartPre = lib.mkOption {
              description = "Service script to start before the program.";
              type = lib.types.nullOr lib.types.path;
              default = null;
            };

            execStart = lib.mkOption {
              description = "Service script to start the program.";
              type = lib.types.path;
            };

            execStop = lib.mkOption {
              description = "Script to run upon service stop.";
              type = lib.types.nullOr lib.types.path;
              default = null;
            };

            enabled = lib.mkOption {
              description = "Whether the service should run on startup.";
              type = lib.types.bool;
              default = false;
            };

            environment = lib.mkOption {
              description = ''
                The environment variables that will be added to the default ones prior to running <option>script</option>
                and <option>finish</option>.
              '';
              type =
                with lib.types;
                attrsOf (oneOf [
                  str
                  int
                ]);
              default = { };
            };

            environmentFile = lib.mkOption {
              description = ''
                The environment file that will be loaded prior to running <option>execStart</option>
                and <option>execStop</option>.
              '';
              type = lib.types.nullOr lib.types.path;
              default = null;
            };

            workingDirectory = lib.mkOption {
              description = ''
                Directory in which both <option>execStart</option> and <option>execStop</option> will be ran.
              '';
              type = lib.types.str;
              default = "/var/empty";
            };

            tmpfiles = lib.mkOption {
              description = ''
                List of nottmpfiles rules, view `lib/generators.nix` for how to use it, or the examples.
              '';
              type = lib.types.unspecified;
              default = [ ];
            };

            user = lib.mkOption {
              description = ''
                The user under which to run the service.
              '';
              type = lib.types.nullOr lib.types.str;
              default = null;
            };

            group = lib.mkOption {
              description = ''
                The group under which to run the service.
              '';
              type = lib.types.nullOr lib.types.str;
              default = null;
            };
          };
        }
      );
      description = "Service definitions.";
      default = { };
    };
  };

  config = {
    # TODO add assertions for this module
    assertions = lib.flatten (
      lib.mapAttrsToList (n: v: [
        {
          assertion =
            let
              selectedCount = (
                lib.count (x: x) (lib.mapAttrsToList (n: v: if v == null then false else true) v.log)
              );
            in
            selectedCount == 1 || selectedCount == 0;
          message = "You can only select one log type, in service ${n}.";
        }
        {
          assertion = (v.log.file.rotate.rotate or 0) >= 0;
          message = "init.service.<name>.log.file.rotate can't be less than 0";
        }
        {
          assertion = v.group == null || v.user != null;
          message = "init.service.<name>.group requires init.service.<name>.user to be set.";
        }
      ]) cfg.services
    );
  };
}
