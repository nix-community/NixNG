nglib:
((nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-nix";

  config = ({ pkgs, ... }: {
    dumb-init = {
      enable = true;
      type.shell = {};
    };
  });
})
