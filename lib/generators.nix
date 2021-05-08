{ lib }:
with lib;
rec {
  toApache = cfg:
    if isAttrs cfg then
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
            abort "Unsupported type in ApacheHTTPD configuration attrset!"
        else if isAttrs value then
          concatStringsSep "\n" (mapAttrsToList (an: av:
            ''
              <${name} ${an}>
                ${toApache av}
              </${name}>
            '') value)
        else
          abort "Unsupported type in ApacheHTTPD configuration attrset!"
      ) cfg)
    else if isList cfg then
      concatMapStringsSep "\n" (x:
        if isAttrs x then
          toApache x
        else if isString x then
          x
        else
          abort "Unsupported type in ApacheHTTPD configuration attrset!"
      ) cfg
    else
      abort "Unsupported type in ApacheHTTPD configuration attrset!";

  toNginx = cfg:
    if isAttrs cfg then
      concatStringsSep "\n" (mapAttrsToList (name: value:
        if isString value then
          "${name} ${value};"
        else if isInt value then
          "${name} ${toString value};"
        else if isStorePath value then
          "${name} ${toString value};"
        else if isList value then
          if all (x: isString x) value then
            "${name} ${concatStringsSep " " value};"
          else if all (x: isInt x) value then
            "${name} ${concatStringsSep " " (toString value)};"
          else if all (x: isStorePath x) value then
            "${name} ${concatStringsSep " " (toString value)};"
          else if all (x: isList x) value then
            concatStringsSep "\n"
              (map (p: "${name} ${concatStringsSep " " p};") value)
          else
            abort "Unsupported type in Nginx configuration attrset!"
        else if isAttrs value then
          concatStringsSep "\n" (mapAttrsToList (an: av:
            ''
              ${name} ${an} {
                ${toNginx av}
              }
            '') value)
        else
          abort "Unsupported type in Nginx configuration attrset!"
      ) cfg)
    else if isList cfg then
      concatMapStringsSep "\n" (x:
        if isAttrs x then
          toNginx x
        else if isString x then
          x
        else
          abort "Unsupported type in Nginx configuration attrset!"
      ) cfg
    else
      abort "Unsupported type in Nginx configuration attrset!";
}
