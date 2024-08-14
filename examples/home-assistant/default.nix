# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-home-assistant";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };

        init.services.home-assistant = {
          shutdownOnExit = true;
        };

        services.home-assistant = {
          enable = true;
          envsubst = true;
          config =
            { default_config = {};
              http.server_port = "8123";
              logger.default = "info";
              homeassistant =
                { name = "Home";
                  latitude = "0";
                  longitude = "0";
                  elevation = "0";
                  # currency = "EUR";
                  unit_system = "metric";
                  time_zone = "Europe/Amsterdam";
                  internal_url = "http://localhost:8123/";
                };
              frontend.themes =
                { };
            };
          package =
            (pkgs.home-assistant.override
              { extraComponents =
                  [ "http"
                    "homeassistant"
                    "image"
                    "person"
                    "cloud"
                    "onboarding"
                    "frontend"
                    "met"
                    "zha"
                    "mobile_app"
                    "dhcp"
                    "logbook"
                    "history"
                    "ssdp"
                    "mqtt"
                  ];
                extraPackages = ps: with ps;
                  [ xmodem
                  ];
              }).overridePythonAttrs (old:
                { doCheck = false;
                });
        };
      };
    });
}
