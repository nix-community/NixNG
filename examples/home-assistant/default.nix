# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, nixpkgs }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-home-assistant";
  config = ({ pkgs, config, lib, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };

        init.services.home-assistant = {
          shutdownOnExit = true;
        };
        init.services.zigbee2mqtt = {
          shutdownOnExit = false;
        };

        services.mosquitto = {
          enable = true;
          config = {};
          envsubst = true;
        };

        services.zigbee2mqtt = {
          enable = true;
          envsubst = true;
          package = (import (pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "0c408a087b4751c887e463e3848512c12017be25";
            sha256 = "sha256-vBVwv3+kPrxbNyfo48cB5cc5/4tq5zlJGas/qw8XNBE=";
          }) { inherit (pkgs.stdenv.hostPlatform) system; }).zigbee2mqtt;

          config =
            { homeassistant = true;
              permit_join = true;
              mqtt = {
                base_topic = "zigbee2mqtt";
                server = "mqtt://localhost:1883";
              };

              frontend =
                { port = 8456;
                  host = "0.0.0.0";
                };

              serial.port = "tcp://\${XIAOMI_HUB_IP}:8888";
              serial.adapter = "ezsp";

              devices = "devices.yaml";
              groups = "groups.yaml";
              log_level = "debug";
            };
        };

        services.home-assistant = {
          enable = true;
          envsubst = true;
          customComponents = {
            xiaomi_gateway3 = pkgs.fetchFromGitHub {
              owner = "AlexxIT";
              repo = "XiaomiGateway3";
              rev = "v1.6.5";
              sha256 = "sha256-RSIJqsbgnktl7zNaxAKUoMjbkrJ1aJTej0vjlCgstJ8=";
            } + "/custom_components/xiaomi_gateway3";
          };
          config =
            { default_config = {};
              http.server_port = "8123";
              logger.default = "info";
              homeassistant =
                { name = "Home";
                  latitude = "\${LATITUDE}";
                  longitude = "\${LONGTIDE}";
                  elevation = "\${ELEVATION}";
                  # currency = "EUR";
                  unit_system = "metric";
                  time_zone = "\${TIMEZONE}";
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
                    "safe_mode"
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
