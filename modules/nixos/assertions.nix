{
  config,
  lib,
  nglib,
  ...
}:
{
  options.nixos = lib.mkOption { type = lib.types.submodule { imports = [ ../assertions.nix ]; }; };

  imports = [
    (nglib.mkOptionsEqual [ "assertions" ] [
      "nixos"
      "assertions"
    ] lib.id)
  ];
}
