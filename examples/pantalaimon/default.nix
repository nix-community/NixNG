nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-pantalaimon";
  config = ({ pkgs, ... }:
    {
      dumb-init = {
        enable = true;
        type.services = {};
      };
      init.services.pantalaimon = {
        shutdownOnExit = true;
      };

      services.pantalaimon = {
        enable = true;

        package = (pkgs.pantalaimon.override
          { enableDbusUi = false; }).overrideAttrs (old: {
              version = "0.10.2";
              src = pkgs.fetchFromGitHub {
                owner = "matrix-org";
                repo = "pantalaimon";
                rev = "0.10.2";
                sha256 = "sha256-sjaJomKMKSZqLlKWTG7Oa87dXa5SnGQlVnrdS707A1w=";
              };
              patches = [];
            });

        config = {
          Default =
            {
              LogLevel = "Debug";
              SSL = "True";
              Notifications = "Off";
            };

          Clockwork =
            {
              Homeserver = "https://matrix.org";
              ListenAddress = "0.0.0.0";
              ListenPort = 80;
              SSL = "False";
            };
        };
      };
    });
}
