/*
 * NixNG
 * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>   
 *  
 *  This file is free software: you may copy, redistribute and/or modify it  
 *  under the terms of the GNU General Public License as published by the  
 *  Free Software Foundation, either version 3 of the License, or (at your  
 *  option) any later version.  
 *  
 *  This file is distributed in the hope that it will be useful, but  
 *  WITHOUT ANY WARRANTY; without even the implied warranty of  
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
 *  General Public License for more details.  
 *  
 *  You should have received a copy of the GNU General Public License  
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.  
 */

{ pkgs, config, lib, nglib, ... }:
with lib;
let
  cfg = config.services.certbot;

  domainOptions = {
    options = {
      extraDomains = mkOption {
        description = "Extra domains to add to the generated certificate.";
        type = with types; listOf str;
        default = [];
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
        default = {};
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
        jobs = let
          script = pkgs.writeShellScript "certbot-renew"
            ''
              ${cfg.package}/bin/certbot renew
            '';
        in
          ["0 0 * * * root ${script}"];
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
      (concatStringsSep "\n" (mapAttrsToList (n: v:
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
        '') cfg.domains));
  };
}
