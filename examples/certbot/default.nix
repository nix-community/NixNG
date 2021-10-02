nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-certbot";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };

        services.certbot = {
          enable = true;

          acceptTerms = true;

          domains = {
            "redalder.org" = {
              extraDomains = [
                "hydra.redalder.org"
              ];
              webroot = "/var/www/acme";
              email = "admin@redalder.org";
            };
          };
        };
      };
    }
  );
}
