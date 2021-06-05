nglib:
((nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-nix";

  config = ({ pkgs, ... }: {
    dumb-init = {
      enable = true;
      type.shell = {};
    };
    nix = {
      enable = true;
      package = pkgs.nixFlakes;
      config = {
        experimental-features = [ "nix-command" "flakes" ]; 
        sandbox = false;
      };
    };
  });
})
