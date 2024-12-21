{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-dnsmasq";

  config =
    { ... }:
    {
      dinit.enable = true;
      init.services.dnsmasq.shutdownOnExit = true;

      services.dnsmasq = {
        enable = true;

        settings = {
          address = [
            "/test1.example.com/192.0.2.42"
            "/test2.example.com/198.51.100.42"
          ];

          server = "203.0.113.42";
          no-resolv = true;
          expand-hosts = true;
        };
      };
    };
}
