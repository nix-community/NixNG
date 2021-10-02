nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-jmusicbot";
  config = ({ pkgs, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.jmusicbot = {
        shutdownOnExit = true;
      };

      services.jmusicbot = {
        enable = true;

        config = {
          prefix = "sudo";
          token = "\${BOT_TOKEN}";
          owner = "\${BOT_OWNER}";
        };
      };
    });
}
