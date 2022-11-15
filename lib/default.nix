lib:
let this =
  { makeSystem = import ./make-system.nix { nglib = this; overlay = import ../overlay;  };
    dag = import ./dag.nix { inherit lib; };
    generators = import ./generators.nix { inherit lib; };
    mkDefaultRec = lib.mapAttrsRecursive (_: v: lib.mkDefault v);
  };
in this
