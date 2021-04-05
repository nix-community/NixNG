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
      };
}
