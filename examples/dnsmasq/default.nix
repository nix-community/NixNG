{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-dnsmasq";

  config = { ... }: {
    dumb-init = {
      enable = true;
      type.services = { };
    };

    init.services.dnsmasq = {
      shutdownOnExit = true;
    };

    services.dnsmasq = {
      enable = true;

      configuration = ''
        address=/pim.example.com/10.42.42.43
      '';
    };
  };
}
