{ fetchurl, lib, callPackage }:
callPackage (import (fetchurl {
  url = "https://raw.githubusercontent.com/nix-community/home-manager/45abf3d38a2b51c00c347cab6950f3734e023bba/modules/lib/dag.nix";
  sha256 = "sha256-NN9iKanf86D1MH9Nx8nsQj9T2+Poy9XeW9pLcZIyFHU=";
})) {}
