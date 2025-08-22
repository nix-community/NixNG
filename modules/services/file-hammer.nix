{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.services.file-hammer;

  ownerOption = {
    user = lib.mkOption {
      type = lib.types.str;
      description = "The owning user of a filesystem node.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      description = "The owning group of a filesystem node.";
    };
  };

  contentOption = lib.mkOption {
    type = lib.types.attrTag {
      ContentAny = lib.mkOption {
        type = lib.types.enum [ [ ] ];
        description = ''
          Leave this file's content unmanaged.
        '';
      };
      ContentText = lib.mkOption {
        type = lib.types.lines;
        description = ''
          Set this file's content to the specified text.
        '';
      };
      ContentBinary = lib.mkOption {
        type = lib.types.str;
        description = ''
          Set this file's content to the specified base64-encoded binary data.
        '';
      };
      ContentFile = lib.mkOption {
        type = lib.types.path;
        description = ''
          Set this file's content to the content of another file.
        '';
      };
    };
    description = ''
      The content of a file, one of:
        - "ContentAny":
          The file will be created but it's content left unmanaged (initially empty).
        - "ContentText":
          The files content will be set to the provided text.
        - "ContentBinary":
          The files content will be set to the provided base64-encoded binary data.
        - "ContentFile":
          The files content will be set to the content of the file specificied. The second file
          is read at application/plan time.
    '';
  };

  fileNodeOption = {
    owner = ownerOption;
    mode = lib.mkOption {
      type = lib.types.int;
      description = ''
        The mode of this file node.
      '';
    };
    content = contentOption;
  };

  directoryNodeOption = {
    owner = ownerOption;
    mode = lib.mkOption {
      type = lib.types.int;
    };
    content = directoryContentOption;
  };

  linkNodeOption = {
    destination = lib.mkOption {
      type = lib.types.path;
      description = ''
        The destination of this link node.
      '';
    };
  };

  directoryContentOption = lib.mkOption {
    type = lib.types.attrTag {
      DirectoryContentManaged = lib.mkOption {
        type = lib.types.submodule {
          options = {
            files = lib.mkOption {
              type = lib.types.attrsOf (lib.types.submodule { options = fileNodeOption; });
              default = { };
              description = ''
                The files that are to exist in this directory's contents.
              '';
            };
            directories = lib.mkOption {
              type = lib.types.attrsOf (lib.types.submodule { options = directoryNodeOption; });
              default = { };
              description = ''
                The directories that are to exist in this directory's contents.
              '';
            };
            links = lib.mkOption {
              type = lib.types.attrsOf (lib.types.submodule { options = linkNodeOption; });
              default = { };
              description = ''
                The links that are to exist in this directory's contents.
              '';
            };
          };
        };
        description = ''
          Manage this directory's contents.
        '';
      };
      DirectoryContentUnmanaged = lib.mkOption {
        type = lib.types.enum [ [ ] ];
        description = ''
          Don't manage this directory's contents, but only ensure it's existence.
        '';
      };
    };

    description = ''
      The content of a directory, one of:
        - "DirectoryContentManaged":
          The directory's content will be managed and ensured to consist of the files, links
          and directories specified
        - "DirectoryContentUnmanaged":
          The directory's content will be left unmanaged, the directory will be created initially
          empty.
    '';
  };

  specificationOption = {
    ignores = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = ''
        Any nodes that are to be completely ignored. See warning for the `specification` data type.
      '';
    };

    directory = directoryNodeOption;
  };

  specFormat = pkgs.formats.json { };

  #########################################
  ## Copied straight from nixpkgs for now #
  #########################################
  # Escape a path according to the systemd rules. FIXME: slow
  # The rules are described in systemd.unit(5) as follows:
  # The escaping algorithm operates as follows: given a string, any "/" character is replaced by "-", and all other characters which are not ASCII alphanumerics, ":", "_" or "." are replaced by C-style "\x2d" escapes. In addition, "." is replaced with such a C-style escape when it would appear as the first character in the escaped string.
  # When the input qualifies as absolute file system path, this algorithm is extended slightly: the path to the root directory "/" is encoded as single dash "-". In addition, any leading, trailing or duplicate "/" characters are removed from the string before transformation. Example: /foo//bar/baz/ becomes "foo-bar-baz".
  escapeSystemdPath =
    s:
    let
      replacePrefix =
        p: r: s:
        (if (lib.hasPrefix p s) then r + (lib.removePrefix p s) else s);
      trim = s: lib.removeSuffix "/" (lib.removePrefix "/" s);
      normalizedPath = lib.strings.normalizePath s;
    in
    lib.replaceStrings [ "/" ] [ "-" ] (
      replacePrefix "." (lib.strings.escapeC [ "." ] ".") (
        lib.strings.escapeC (lib.stringToCharacters " !\"#$%&'()*+,;<=>=@[\\]^`{|}~-") (
          if normalizedPath == "/" then normalizedPath else trim normalizedPath
        )
      )
    );
in
{
  options.services.file-hammer = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { config, ... }:
        {
          options = {
            specification = specificationOption;

            specificationFile = lib.mkOption {
              type = specFormat.type;
              internal = true;
            };
          };

          config = {
            specificationFile = specFormat.generate "specification.json" config.specification;
          };
        }
      )
    );
    default = { };
  };

  config.init.services = lib.mapAttrs' (
    name: cfg:
    lib.nameValuePair "file-hammer@${escapeSystemdPath name}" {
      enabled = true;

      environment = {
        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      };

      type = "scripted";

    execStart = pkgs.writeShellScript "environment-etc" ''
      mkdir "${name}"
      ${lib.getExe pkgs.fileHammer} --root "${name}" apply --configuration ${cfg.specificationFile}
    '';
  }) cfg;
}
