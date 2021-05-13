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

      postScript = mkOption {
        description = ''
          A shell script to run after certbot.
        '';
        type = types.nullOr types.str;
        default = null;
      };
    };
  };

  scripts = mapAttrsToList (n: v:
    pkgs.writeShellScript n ''
      ${cfg.package}/bin/certbot \
        --webroot \
        -w ${v.webroot} \
        -d ${n} \
        ${concatMapStringsSep " " (d: "-d " + d) v.extraDomains}

        ${optionalString (v.postScript != null)
          "${v.postScript}"}
      '') cfg.domains;
in
{
  options = {
    services.certbot = {
      enable = mkEnableOption "Enable certbot, certificate management tool.";
      package = mkOption {
        description = "certbot package.";
        type = types.package;
        default = pkgs.nginx;
      };

      domains = mkOption {
        description = "Domains for which, certbot will fetch certificates.";
        type = with types; attrsOf (submodule domainOptions);
        default = {};
      };
    }; 
  };

  config = {
    services.crond = {
      enable = mkDefault true;

      crontabs.certbot = {
        jobs = map (script: "0 0 * * * root ${script}") scripts;
      };
    };

    system.activation.certbot = nglib.dag.dagEntryAfter
      [ "createBaseEnv" "users" ]
      (concatStringsSep "\n" scripts);
  };
}
