nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-postfix";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = {};
        };
        init.services.postfix = {
          shutdownOnExit = true;
        };
        services.socklog = {
          enable = true;
          unix = "/dev/log";
        };
        services.postfix = {
          enable = true;

          masterConfig = {
          };

          mainConfig =
              {
                compatibility_level = 3;
                mydomain = "example.org";
                myorigin = "$mydomain";
                myhostname = "mail.example.org";

                alias_maps = "inline:{ root=root }";

                mydestination =
                  [ "localhost"
                    "mail.example.org"
                    "example.org"
                  ];

                mynetworks_style = "host";
                relay_domains = "";
              };
        };
      };
    });
}
