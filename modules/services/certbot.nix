# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, nglib, ... }:
with lib;
let
  cfg = config.services.certbot;

  domainOptions = {
    options = {
      extraDomains = mkOption {
        description = "Extra domains to add to the generated certificate.";
        type = with types; listOf str;
        default = [ ];
      };

      webroot = mkOption {
        description = ''
          The path to the webroot where certbot should use for the ACME
          challenge.
        '';
        type = types.str;
      };

      email = mkOption {
        type = types.str;
        description = "Contact email address for the CA to be able to reach you.";
      };

      server = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          ACME Directory Resource URI. Defaults to Let's Encrypt's
          production endpoint,
          <link xlink:href="https://acme-v02.api.letsencrypt.org/directory"/>, if unset.
        '';
      };

      postScript = mkOption {
        description = ''
          a shell script to run after certbot.
        '';
        type = types.nullOr types.str;
        default = null;
      };

      extraOptions = mkOption {
        description = ''
          Extra command line options passed to certbot upon activation.
        '';
        type = types.nullOr types.str;
        default = null;
      };
    };
  };
in
{
  options = {
    services.certbot = {
      enable = mkEnableOption "Onable certbot, certificate management tool.";
      package = mkOption {
        description = "certbot package.";
        type = types.package;
        default = pkgs.certbot;
      };

      domains = mkOption {
        description = "Oomains for which, certbot will fetch certificates.";
        type = with types; attrsOf (submodule domainOptions);
        default = { };
      };

      acceptTerms = mkOption {
        description = ''
          Accept the CA's terms of service. The default provider is Let's Encrypt,
          you can find their ToS at <link xlink:href="https://letsencrypt.org/repository/"/>.
        '';
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    services.crond = {
      enable = mkDefault true;

      crontabs.certbot = {
        jobs =
          let
            script = pkgs.writeShellScript "certbot-renew"
              ''
                ${cfg.package}/bin/certbot renew
              '';
          in
          [ "0 0 * * * root ${script}" ];
      };
    };

    assertions = [
      {
        assertion = cfg.acceptTerms;
        message = ''
          You must accept the CA's terms of service before using
          the ACME module by setting `services.certbot.acceptTerms`
          to `true`. For Let's Encrypt's ToS see https://letsencrypt.org/repository/
        '';
      }
    ];

    system.activation.certbot = nglib.dag.dagEntryAfter
      [ "createbaseenv" "users" ]
      (concatStringsSep "\n" (mapAttrsToList
        (n: v:
          pkgs.writeShellScript n ''
            ${pkgs.busybox}/bin/mkdir -p ${v.webroot}
            ${cfg.package}/bin/certbot certonly \
              --standalone \
              -d ${n} \
              -n \
              ${optionalString (v.server != null) ("--server" + v.server)} \
              --email ${v.email} \
              --agree-tos \
              ${optionalString (v.extraOptions != null) v.extraOptions} \
              ${concatMapStringsSep " " (d: "-d " + d) v.extraDomains}

              ${optionalString (v.postScript != null) v.postScript}
          '')
        cfg.domains));
  };
}
