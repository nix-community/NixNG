nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-crond";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init.enable = true;
        services.crond = {
          enable = true;
          crontabs = {
            hydra = {
              environment = {
                PATH = "${pkgs.busybox}/bin";
              };
              jobs = [
                ''*/2 * * * * root echo "asd"''
              ];
            };
          };
        };
      };
    }
  );
}
