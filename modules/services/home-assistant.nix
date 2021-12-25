# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.home-assistant;
  format = pkgs.formats.yaml {};

  configDir = pkgs.runCommandNoCC "home-assistant-config-dir" {}
    ''
      mkdir -p $out
      ln -s ${format.generate "configuration.yaml" cfg.config} $out/configuration.yaml
    '';
in
{
  options.services.home-assistant = {
    enable = mkEnableOption "Enable Home Assistant";

    package = mkOption {
      type =  with types; package;
      default = pkgs.home-assistant;
      description = ''
        Which home-assistant package to use. Adding components is a shorthand for adding python packages.
      '';
    };

    customComponents = mkOption {
      type = with types; attrsOf (oneOf [ package str ]);
      default = {};
      description = ''
        Extra components to be installed into <literal>/run/home-assistant/custom_components</custom_components>.
      '';
    };

    config = mkOption {
      type = format.type;
      default = {};
      description = ''
        Home Assistant configuration, <link>https://www.home-assistant.io/docs/configuration/</link>.
      '';
    };
  };

  config = {
    services.home-assistant.config = mapAttrsRecursive (_: v: mkDefault v)
      {
        http.server_port = "8123";
      };

    init.services.home-assistant = {
      script = pkgs.writeShellScript "home-assistant-run"
        ''
          mkdir -p /run/home-assistant/ /var/home-assistant/
          cp '${configDir}'/* /run/home-assistant/
          ln -s /var/home-assistant /run/home-assistant/.storage

          ${if cfg.customComponents != {} then "mkdir /run/home-assistant/custom_components" else ""}
          ${concatStringsSep "\n" (mapAttrsToList (n: v: "ln -s ${v} /run/home-assistant/custom_components/${n}") cfg.customComponents)}

          ln -s /var/home-assistant/zigbee.db /run/home-assistant/zigbee.db

          ${cfg.package}/bin/hass --runner --config /run/home-assistant
        '';
      enabled = true;
    };
  };
}
