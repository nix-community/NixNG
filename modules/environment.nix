# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  pkgs,
  lib,
  config,
  nglib,
  ...
}:
let
  cfg = config.environment;

  toIntBase8 =
    str:
    lib.pipe str [
      lib.stringToCharacters
      (map lib.toInt)
      (lib.foldl (acc: digit: acc * 8 + digit) 0)
    ];

  configFile = lib.mkMerge (
    lib.mapAttrsToList (
      name: value:
      let
        genDir =
          path: final:
          (
            if path == [ ] then
              final
            else
              {
                directories.${lib.head path} = {
                  content."DirectoryContentManaged" = genDir (lib.tail path) final;
                  mode = 509;
                  owner = {
                    user."UserName" = "root";
                    group."GroupName" = "root";
                  };
                };
              }
          );

        segments = lib.filter (x: x != "") (lib.splitString "/" name);
        path = lib.init segments;
        final = lib.last segments;
      in
      if value.enable then
        genDir path (
          if value.mode == "symlink" then
            {
              links.${final} = {
                destination = value.source;
              };
            }
          else
            {
              files.${final} = {
                content."ContentFile" = value.source;
                mode = toIntBase8 value.mode;
                owner = {
                  user = {
                    ${if value.user != null then "UserName" else null} = value.user;
                    ${if value.uid != null then "UserId" else null} = value.uid;
                  };
                  group = {
                    ${if value.group != null then "GroupName" else null} = value.group;
                    ${if value.gid != null then "UserId" else null} = value.gid;
                  };
                };
              };
            }
        )
      else
        { }

    ) cfg.etc
  );

  etcEntry =
    { name, config, ... }:
    {
      options = {
        enable = lib.mkOption {
          type = lib.types.bool;
          description = ''
            Whether this /etc file should be generated. This option allows specific /etc files to be disabled.
          '';
          default = true;
        };

        source = lib.mkOption {
          type = lib.types.path;
          description = ''
            Path of the source file.
          '';
          default = pkgs.writeText (lib.replaceString "/" "-" name) config.text;
        };

        text = lib.mkOption {
          type = lib.types.lines;
          description = ''
            Text content.
          '';
        };

        target = lib.mkOption {
          type = lib.types.str;
          description = ''
            Name of symlink (relative to `/etc`). Defaults to the attribute name.
          '';
          default = name;
        };

        mode = lib.mkOption {
          type = lib.types.str;
          description = ''
            If set to something else than symlink, the file is copied instead of symlinked, with the given file mode.
          '';
          default = "symlink";
        };

        user = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          description = ''
            User name of file owner.

            Only takes effect when the file is copied (that is, the mode is not symlink).
          '';
          default = "root";
        };

        group = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          description = ''
            Group name of file owner.

            Only takes effect when the file is copied (that is, the mode is not symlink).
          '';
          default = "root";
        };

        uid = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          description = ''
            UID of created file. Only takes effect when the file is copied (that is, the mode is not 'symlink').
          '';
          default = null;
        };

        gid = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          description = ''
            GID of created file. Only takes effect when the file is copied (that is, the mode is not ‘symlink’).
          '';
          default = null;
        };
      };
    };
in
{
  options.environment = {
    hammer = lib.mkOption {
      type = (pkgs.formats.json { }).type;
    };

    hammerFile = lib.mkOption {
      type = lib.types.path;
      default = (pkgs.formats.json { }).generate "fila-hammer.json" cfg.hammer;
    };

    variables = lib.mkOption {
      default = { };
      example = {
        EDITOR = "vim";
        BROWSER = "firefox";
      };
      type = with lib.types; attrsOf (either str (listOf str));
      apply = nglib.mkApply (
        x:
        lib.concatStringsSep " " (
          lib.mapAttrsToList (n: v: "${n}=" + (if lib.isList v then lib.concatStringsSep ":" v else v)) x
        )
      );
    };

    shell = {
      enable = lib.mkOption {
        description = ''
          Enable the packages needed to get a shell.
        '';
        type = lib.types.bool;
        default = true;
      };
      profile = lib.mkOption {
        description = ''
          Shell script fragments, concataned into /etc/profile.
        '';
        type = with lib.types; listOf str;
        apply = nglib.mkApply (x: pkgs.writeShellScript "profile" (lib.concatStringsSep "\n" x));
        default = [ ];
      };
    };

    createBaseEnv = lib.mkOption {
      description = ''
        Create /bin/sh, /usr/bin/env, /var/empty, and /tmp.
      '';
      type = lib.types.bool;
      default = true;
    };

    systemPackages = lib.mkOption {
      description = "Packages globally added to PATH";
      default = [ ];
      type = with lib.types; listOf package;
    };

    etc = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule etcEntry);
      default = { };
    };
  };

  config = {
    services.file-hammer."/etc".specification = {
      directory = {
        mode = 509;
        owner = {
          user."UserName" = "root";
          group."GroupName" = "root";
        };
        content."DirectoryContentManaged" = lib.mkMerge [
          configFile
          {
            files = {
              "hostname" = {
                content."ContentAny" = [ ];
                mode = 420;
                owner = {
                  group."GroupName" = "root";
                  user."UserName" = "root";
                };
              };
              "resolv.conf" = {
                content."ContentAny" = [ ];
                mode = 420;
                owner = {
                  group."GroupName" = "root";
                  user."UserName" = "root";
                };
              };
              # "shadow" = {
              #   content."ContentAny" = [ ];
              #   mode = 256;
              #   owner = {
              #     group."GroupName" = "root";
              #     user."UserName" = "root";
              #   };
              # };
            };
          }
        ];
      };
      ignores = [
        "group"
        "mtab"
        "passwd"
        "shadow"
      ];
    };
    environment.etc."profile" = {
      source = cfg.shell.profile.applied;
      mode = "symlink";
    };
    environment.systemPackages = with pkgs; [ busybox ];

    environment.shell.profile = [
      ''
        ${lib.optionalString (cfg.variables.applied != "") "export ${cfg.variables.applied}"}
        export PATH="$PATH"':${lib.makeBinPath cfg.systemPackages}'
      ''
    ];

    system.activation.createBaseEnv = lib.mkIf cfg.createBaseEnv (
      nglib.dag.dagEntryAnywhere ''
        export PATH=${pkgs.busybox}/bin

        # Borrowed from NixOS therefore it's licensed under the MIT license
        #### Activation script snippet usrbinenv:
        _localstatus=0
        mkdir -m 0755 -p /usr/bin
        ln -sfn ${pkgs.busybox}/bin/env /usr/bin/.env.tmp
        mv /usr/bin/.env.tmp /usr/bin/env # atomically replace /usr/bin/env

        # Create the required /bin/sh symlink; otherwise lots of things
        # (notably the system() syscall) won't work.
        mkdir -m 0755 -p /bin
        ln -sfn "${pkgs.busybox}/bin/sh" /bin/.sh.tmp
        mv /bin/.sh.tmp /bin/sh # atomically replace /bin/sh

        mkdir -pm 0777 /tmp
        mkdir -pm 0555 /var/empty
      ''
    );
  };
}
