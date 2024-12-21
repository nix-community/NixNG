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
      user: group: script:
      if user != null then
        "chpst -u ${user}${lib.optionalString (group != null) ":${group}"} ${script}"
      else
        script;
  };
in
this
