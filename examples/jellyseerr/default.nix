{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-jellyseerr";

  config =
    { ... }:
    {
      dinit.enable = true;
      init.services.jellyseerr.shutdownOnExit = true;
      services.jellyseerr.enable = true;
    };
}
