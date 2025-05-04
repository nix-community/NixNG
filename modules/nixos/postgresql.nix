{
  nglib,
  lib,
  config,
  ...
}:
{
  options = {
    nixos.services.postgresql = lib.mkOption {
      type = lib.types.unspecified;
      default = { };
      description = ''
        Enable NixOS compatible PostgreSQL module. This module should have the same
        semantics as the upstream module, as such see
        [the upstream options](https://search.nixos.org/options?query=services.postgresql).

        WARNING: this module implements a rather complete subset of the upstream
        NixOS module, but even then, it's highly experimental and may have semantic
        differences.
      '';
    };
  };

  imports = [
    (nglib.mkOptionsEqual
      [
        "services"
        "postgresql"
      ]
      [
        "nixos"
        "services"
        "postgresql"
      ]
      lib.id
    )
  ];
}
