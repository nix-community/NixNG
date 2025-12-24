lib:
let
  inherit (lib) types;
  this = {
    makeSystem = import ./make-system.nix {
      nglib = this;
      overlay = import ../overlay;
    };
    dag = import ./dag.nix { inherit lib; };
    generators = import ./generators.nix { inherit lib; };
    mkDefaultRec = lib.mapAttrsRecursive (_: v: lib.mkDefault v);
    mkApply = fun: x: {
      original = x;
      applied = fun x;
    };

    mkDagOption =
      description:
      lib.mkOption {
        inherit description;
        type = types.attrsOf (
          types.submodule {
            options = {
              data = lib.mkOption {
                description = ''
                  Script fragment which to run.
                '';
                type = types.str;
              };
              before = lib.mkOption {
                description = ''
                  Script before dependencies. See <literal>/lib/dag.nix</literal>.
                '';
                type = with types; listOf str;
              };
              after = lib.mkOption {
                description = ''
                  Script after dependencies. See <literal>/lib/dag.nix</literal>
                '';
                type = with types; listOf str;
              };
            };
          }
        );
        apply = this.dag.dagTopoSort;
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

    nottmpfiles = import ./nottmpfiles {
      inherit lib;
      nglib = this;
    };

    maybeChangeUserAndGroup =
      user: group: supp: script:
      if user != null then
        let
          group' = if group != null then "${group}" else ":";
        in
        "setgroups ${user} ${group'} :${lib.concatStringsSep ":" supp} ${script}"
      else
        script;
  };
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
this
