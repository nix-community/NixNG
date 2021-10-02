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

{ lib }:
with lib;
rec {
  toApache = cfg:
    if isAttrs cfg then
      concatStringsSep "\n"
        (mapAttrsToList
          (name: value:
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
              concatStringsSep "\n"
                (mapAttrsToList
                  (an: av:
                    ''
                      <${name} ${an}>
                        ${toApache av}
                      </${name}>
                    '')
                  value)
            else
              abort "Unsupported type in ApacheHTTPD configuration attrset!"
          )
          cfg)
    else if isList cfg then
      concatMapStringsSep "\n"
        (x:
          if isAttrs x then
            toApache x
          else if isString x then
            x
          else
            abort "Unsupported type in ApacheHTTPD configuration attrset!"
        )
        cfg
    else
      abort "Unsupported type in ApacheHTTPD configuration attrset!";

  toNginx = cfg:
    if isAttrs cfg then
      concatStringsSep "\n"
        (mapAttrsToList
          (name: value:
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
              concatStringsSep "\n"
                (mapAttrsToList
                  (an: av:
                    ''
                      ${name} ${an} {
                        ${toNginx av}
                      }
                    '')
                  value)
            else
              abort "Unsupported type in Nginx configuration attrset!"
          )
          cfg)
    else if isList cfg then
      concatMapStringsSep "\n"
        (x:
          if isAttrs x then
            toNginx x
          else if isString x then
            x
          else
            abort "Unsupported type in Nginx configuration attrset!"
        )
        cfg
    else
      abort "Unsupported type in Nginx configuration attrset!";

  toDovecot = cfg:
    if isAttrs cfg then
      concatStringsSep "\n"
        (mapAttrsToList
          (name: value:
            if isNull value then
              ""
            else if isString value then
              "${name} = ${value}"
            else if isInt value then
              "${name} = ${toString value}"
            else if isStorePath value then
              "${name} = ${toString value}"
            else if isBool value then
              if value then
                "${name} = yes"
              else
                "${name} = no"

            else if isAttrs value then
              concatStringsSep "\n"
                (mapAttrsToList
                  (n: v:
                    ''
                      ${name} ${if n == "" then n else "\"" + n + "\""} {
                    ''
                    +
                    toDovecot v
                    +
                    ''

              }
            ''
                  )
                  value)
            else if isList value && name != "include'" && name != "include_try'" then
              "${name} = " + concatMapStringsSep ", "
                (x:
                  if isString x then
                    x
                  else if isInt x then
                    toString x
                  else if isStorePath x then
                    toString x
                  else if isBool x then
                    if value then
                      "yes"
                    else
                      "no"
                  else
                    abort "Unsupported type in Dovecot configuration attrset!"
                )
                value
            else if isList value && name == "include'" then
              concatMapStringsSep "\n"
                (x:
                  "include! " + x
                )
                value
            else if isList value && name == "include_try'" then
              concatMapStringsSep "\n"
                (x:
                  "include_try! " + x
                )
                value
            else
              abort "Unsupported type in Dovecot configuration attrset!"
          )
          cfg)
    else
      abort "Unsupported type in Dovecot configuration attrset!";

  postfix = {
    toMainCnf = cfg:
      if isAttrs cfg then
        concatStringsSep "\n"
          (mapAttrsToList
            (name: value:
              if isNull value then
                ""
              else if isString value then
                "${name} = ${value}"
              else if isInt value then
                "${name} = ${toString value}"
              else if isStorePath value then
                "${name} = ${toString value}"
              else if isBool value then
                if value then
                  "${name} = yes"
                else
                  "${name} = no"
              else if isList value then
                "${name} = " + concatMapStringsSep ", "
                  (x:
                    if isString x then
                      x
                    else if isInt x then
                      toString x
                    else if isStorePath x then
                      toString x
                    else if isBool x then
                      if value then
                        "yes"
                      else
                        "no"
                    else
                      abort "Unsupported type in Postfix main configuration attrset!"
                  )
                  value
              else
                abort "Unsupported type in Postfix main configuration attrset!"
            )
            cfg)
      else
        abort "Unsupported type in Postfix main configuration attrset!";
  };

  php = {
    ini = cfg:
      concatStringsSep "\n" (mapAttrsToList
        (name: value:
          "${name} = ${toString value}"
        )
        cfg);
    fpm = env: cfg: header:
      concatStringsSep "\n"
        ([ "[${header}]" ]
          ++ (mapAttrsToList
          (name: value:
            "${name} = ${toString value}"
          )
          cfg)
          ++ (mapAttrsToList
          (name: value:
            "env[${name}] = ${toString value}"
          )
          env)
          ++ [ "" ]);
  };
}
