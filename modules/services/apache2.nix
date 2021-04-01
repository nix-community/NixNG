{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.apache2;

  configParser = config:
    concatStringsSep "\n" (mapAttrsToList (name: value:
      if isString value then
        "${name} ${value}"
      else if isInt value then
        "${name} ${toString value}"
      else if isStorePath value then
        "${name} ${toString value}"
      else if isList value then
        if all (x: isString x) value then
          "${name} ${concatStringsSep " " value}"
        else if all (x: isInt x) value then
          "${name} ${concatStringsSep " " (toString value)}"
        else if all (x: isStorePath x) value then
          "${name} ${concatStringsSep " " (toString value)}"
        else if all (x: isList x) value then
          concatStringsSep "\n"
            (map (p: "${name} ${concatStringsSep " " p}") value)
        else
          abort "Unsupported type in ApacheHTTPD configuration attrset, the module system should have caught this!"
      else if isAttrs value then
        concatStringsSep "\n" (mapAttrsToList (an: av:
          ''
            <${name} ${an}>
              ${configParser av}
            </${name}>
          '') value)
      else
          abort "Unsupported type in ApacheHTTPD configuration attrset, the module system should have caught this!"
    ) config);
in
{
  options.services.apache2 = {
    enable = mkEnableOption "Enable Apache2 http server";
    configuration = mkOption rec {
      description = "Apache2 configuration";
      type = with types;
          types.attrsOf (oneOf [
            str
            int
            (listOf (oneOf [ str int (listOf (oneOf [ str int ])) ]))
            (attrsOf type)
          ]);
    };
  };

  config.init.services.apache2 = let
    config = pkgs.writeText "apache.cfg" (configParser cfg.configuration);
  in
    mkIf cfg.enable
      {
        script = pkgs.writeShellScript "apache2-run"
          ''
            ${pkgs.apacheHttpd}/bin/httpd -f ${config} -DFOREGROUND 2>&1
          '';
      };
}
