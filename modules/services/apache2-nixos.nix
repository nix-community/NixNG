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

{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.apache2;

  nixosImport = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/MagicRB/nixpkgs/832f37652d21d6f783ae93d1cc33cd1485fa1640/nixos/modules/services/web-servers/apache-httpd2/separate.nix";
    sha256 = "1+ZdOqOdNt6N/dR99LuL0+WfTwgcGuDlXD89PD1YPvs=";
  };
  inherit (import nixosImport { inherit lib; }) configParser runtimeDir;
  
  functionTo = 
    elemType: mkOptionType {
      name = "functionTo";
      description = "function that evaluates to a(n) ${elemType.name}";
      check = isFunction;
      merge = loc: defs:
        fnArgs: (mergeDefinitions (loc ++ [ "[function body]" ]) elemType (map (fn: { inherit (fn) file; value = fn.value fnArgs; }) defs)).mergedValue;
      getSubOptions = elemType.getSubOptions;
      getSubModules = elemType.getSubModules;
      substSubModules = m: functionTo (elemType.substSubModules m);
    };
in
{
  options = {
    services.apache2 = {
      enable = mkEnableOption "Enable Apache2 http server";
      package = mkOption {
        description = "Apache2 package";
        type = types.package;
        default = pkgs.apacheHttpd;
      };
      configuration = mkOption {
        description = "Apache2 configuration";
        type = with types;
          let
            self = 
              oneOf [
                (attrsOf (oneOf [
                  str
                  int
                  (listOf (oneOf [ str int (listOf (oneOf [ str int ])) ]))
                  (attrsOf self)
                ]))
                (listOf (oneOf [ str self]))
              ];
          in
            self // { description = "loop breaker"; };
      };
    };
  };


  config.init.services.apache2 = let
    config = pkgs.writeText "apache.cfg" (configParser cfg.configuration);
  in
    mkIf cfg.enable
      {
        script = pkgs.writeShellScript "apache2-run"
          ''
            ${cfg.package}/bin/httpd -f ${config} -DFOREGROUND 2>&1
          '';
        enabled = true;
      };
}
