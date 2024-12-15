{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-ntfy-sh";

  config =
    { ... }:
    {
      dinit.enable = true;
      init.services.ntfy-sh.shutdownOnExit = true;

      services.ntfy-sh = {
        enable = true;

        settings = {
          base-url = "https://ntfy.example.com";
          attachment-file-size-limit = "30M";
          enable-metrics = true;
        };
      };
    };
}
