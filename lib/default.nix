lib:
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
      user: group: script:
      if user != null then
        "chpst -u ${user}${lib.optionalString (group != null) ":${group}"} ${script}"
      else
        script;

    inherit (import ./options.nix args) mkUserOption mkGroupOption;

    makeSystem = import ./make-system.nix { inherit lib nglib overlay; };

  }
)
