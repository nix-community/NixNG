lib:
let this =
  { makeSystem = import ./make-system.nix { nglib = this; overlay = import ../overlay;  };
    dag = import ./dag.nix { inherit lib; };
    generators = import ./generators.nix { inherit lib; };
    mkDefaultRec = lib.mapAttrsRecursive (_: v: lib.mkDefault v);
    mkApply = fun: x:
      {
        original = x;
        applied = fun x;
      };

    mkTmpfilesOption = name: description:
      lib.mkOption {
        default = {};
        type = with lib.types;
          attrsOf (listOf (listOf str));
        description = ''
          A lists of lists. The first list corresponds to the individual entries
          and the most inner one corresponds to the individual fields in use by `tmpfiles.conf`.

          ${description}
        '';
        example = ''
          [
            # Type  Path          Mode   User   Group   Age   Argument...
            [ "d"     "/run/user"   "0755" "root" "root"  "10d" "-" ]
            [ "L"     "/tmp/foobar" "-"    "-"    "-"     "-"   "/dev/null" ]
          ]
        '';
        apply = with lib;
          this.mkApply (x:
            pkgs.writeText name
              (concatMapStringsSep "\n" (concatStringsSep " ")));
      };

    mkDagOption = description:
      lib.mkOption {
        inherit description;
        type = with lib.types; attrsOf (submodule {
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
        });
        apply = this.dag.dagTopoSort;
        default = { };
      };

    mergeShellFragmentsIsolated = fragments: ''
        _status=0
        trap "_status=1 _localstatus=\$?" ERR

        ${lib.concatStringsSep "\n" (map (dag:
          ''
            _localstatus=0
            echo "Running fragment ${dag.name}"
            (
              ${dag.data}
            )
            if expr "$_localstatus" > 0; then
              printf "Fragment '%s' failed (%s)\n" "${dag.name}" "$_localstatus"
            fi
          ''
        ) fragments)}
    '';
  };
in this
