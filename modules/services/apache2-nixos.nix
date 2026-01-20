# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  pkgs,
  config,
  lib,
  nglib,
  ...
}:
let
  cfg = config.services.apache2;
  runtimeConfig = "/run/cfg/apache.cfg";

  inherit (nglib.generators) toApache;
in
{
  options = {
    services.apache2 = {
      enable = lib.mkEnableOption "Enable Apache2 http server";
      package = lib.mkOption {
        description = "Apache2 package";
        type = lib.types.package;
        default = pkgs.apacheHttpd;
      };
      createUserGroup = lib.mkOption {
        description = ''
          Whether to create the default user <literal>www-data</literal>
          and group <literal>www-data</literal>.
        '';
        type = lib.types.bool;
        default = true;
      };
      envsubst = lib.mkEnableOption "Run envsubst on the configuration file.";
      configuration = lib.mkOption {
        description = "Apache2 configuration";
        type =
          with lib.types;
          let
            self = oneOf [
              (attrsOf (oneOf [
                str
                int
                (listOf (oneOf [
                  str
                  int
                  (listOf (oneOf [
                    str
                    int
                  ]))
                ]))
                (attrsOf self)
              ]))
              (listOf (oneOf [
                str
                self
              ]))
            ];
          in
          self // { description = "loop breaker"; };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    init.services.apache2 =
      let
        config = pkgs.writeText "apache2.cfg" (toApache cfg.configuration);
      in
      {
        script = pkgs.writeShellScript "apache2-run" (
          if cfg.envsubst then
            ''
              export PATH=${pkgs.envsubst}/bin:$PATH

              mkdir -p /run/cfg
              install -o www-data -g www-data -m 0440 /dev/null ${runtimeConfig}
              envsubst < ${config} > ${runtimeConfig}

              HOME=~www-data ${cfg.package}/bin/httpd \
                -f ${runtimeConfig} -DFOREGROUND 2>&1
            ''
          else
            ''
              HOME=~www-data ${cfg.package}/bin/httpd \
                -f ${config} -DFOREGROUND 2>&1
            ''
        );
        enabled = true;
      };

    environment.systemPackages = [ cfg.package ];

    users.users."www-data" = lib.mkIf cfg.createUserGroup {
      description = "Apache HTTPD";
      group = "www-data";
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.www-data;
    };

    users.groups."www-data" = lib.mkIf cfg.createUserGroup { gid = config.ids.gids.www-data; };
  };
}
