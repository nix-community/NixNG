{ lib, inputs }:
lib.fix (
  nglib:
  let
    overlay = import ../overlay;
    args = {
      inherit lib nglib overlay;
    };
  in
  {
    dag = import ./dag.nix args;
    generators = import ./generators.nix args;
    mkDefaultRec = lib.mapAttrsRecursive (_: v: lib.mkDefault v);
    mkApply = fun: x: {
      original = x;
      applied = fun x;
    };

    mkDagOption =
      description:
      lib.mkOption {
        inherit description;
        type = lib.types.attrsOf (
          lib.types.submodule {
            options = {
              data = lib.mkOption {
                description = ''
                  Script fragment which to run.
                '';
                type = lib.types.str;
              };
              before = lib.mkOption {
                description = ''
                  Script before dependencies. See <literal>/lib/dag.nix</literal>.
                '';
                type = lib.types.listOf lib.types.str;
              };
              after = lib.mkOption {
                description = ''
                  Script after dependencies. See <literal>/lib/dag.nix</literal>
                '';
                type = lib.types.listOf lib.types.str;
              };
            };
          }
        );
        apply = nglib.dag.dagTopoSort;
        default = { };
      };

    mergeShellFragmentsIsolated = fragments: ''
      _status=0
      trap "_status=1 _localstatus=\$?" ERR

      ${lib.concatStringsSep "\n" (
        map (dag: ''
          _localstatus=0
          echo "Running fragment ${dag.name}"
          (
            ${dag.data}
          )
          if expr "$_localstatus" > 0; then
            printf "Fragment '%s' failed (%s)\n" "${dag.name}" "$_localstatus"
          fi
        '') fragments
      )}
    '';
    nottmpfiles = import ./nottmpfiles args;

    maybeChangeUserAndGroup =
      user: group: supp: script:
      if user != null then
        let
          group' = if group != null then "${group}" else ":";
        in
        "setgroups ${user} ${group'} :${lib.concatStringsSep ":" supp} ${script}"
      else
        script;

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

    errorExperimentalNixOS =
      config:
      lib.throwIfNot (config.nixos.acceptRisks == "I accept the risks") ''
        NixOS module compatibility is highly experimental, severely unfinished and most definitely has
        functional and security bugs. Unless you know what you're doing and are willing to accept the risks
        reconsider it's usage. To signify you are aware of these risks, set the option
        `config.nixos.acceptRisks` to `"I accept the risks"`.

        If you run into any of the aforementioned deficiencies please reach out on Matrix at
        `#nixng:matrix.redalder.org`.
      '';

    inherit (import ./options.nix args)
      mkUserOption
      mkGroupOption
      mkOptionsEqual
      getOptionFromPath
      ;

    makeSystem = import ./make-system.nix { inherit nglib overlay inputs; };
  }
)
