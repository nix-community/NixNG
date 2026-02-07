# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, ... }:
let
  inherit (lib)
    isAttrs
    concatStringsSep
    concatMapStringsSep
    mapAttrsToList
    isString
    isInt
    isStorePath
    isList
    isBool
    all
    ;
in
rec {
  toApache =
    cfg:
    if isAttrs cfg then
      concatStringsSep "\n" (
        mapAttrsToList (
          name: value:
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
              concatStringsSep "\n" (map (p: "${name} ${concatStringsSep " " p}") value)
            else
              abort "Unsupported type in ApacheHTTPD configuration attrset!"
          else if isAttrs value then
            concatStringsSep "\n" (
              mapAttrsToList (an: av: ''
                <${name} ${an}>
                  ${toApache av}
                </${name}>
              '') value
            )
          else
            abort "Unsupported type in ApacheHTTPD configuration attrset!"
        ) cfg
      )
    else if isList cfg then
      concatMapStringsSep "\n" (
        x:
        if isAttrs x then
          toApache x
        else if isString x then
          x
        else
          abort "Unsupported type in ApacheHTTPD configuration attrset!"
      ) cfg
    else
      abort "Unsupported type in ApacheHTTPD configuration attrset!";

  toNginx =
    cfg:
    if isAttrs cfg then
      concatStringsSep "\n" (
        mapAttrsToList (
          name: value:
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
              concatStringsSep "\n" (map (p: "${name} ${concatStringsSep " " p};") value)
            else
              abort "Unsupported type in Nginx configuration attrset!"
          else if isAttrs value then
            concatStringsSep "\n" (
              mapAttrsToList (an: av: ''
                ${name} ${an} {
                  ${toNginx av}
                }
              '') value
            )
          else
            abort "Unsupported type in Nginx configuration attrset!"
        ) cfg
      )
    else if isList cfg then
      concatMapStringsSep "\n" (
        x:
        if isAttrs x then
          toNginx x
        else if isString x then
          x
        else
          abort "Unsupported type in Nginx configuration attrset!"
      ) cfg
    else
      abort "Unsupported type in Nginx configuration attrset!";

  toDovecot =
    cfg:
    if isAttrs cfg then
      concatStringsSep "\n" (
        mapAttrsToList (
          name: value:
          if isNull value then
            ""
          else if isString value then
            "${name} = ${value}"
          else if isInt value then
            "${name} = ${toString value}"
          else if isStorePath value then
            "${name} = ${toString value}"
          else if isBool value then
            if value then "${name} = yes" else "${name} = no"

          else if isAttrs value then
            concatStringsSep "\n" (
              mapAttrsToList (
                n: v:
                ''
                  ${name} ${if n == "" then n else "\"" + n + "\""} {
                ''
                + toDovecot v
                + ''

                  }
                ''
              ) value
            )
          else if isList value && name != "include'" && name != "include_try'" then
            "${name} = "
            + concatMapStringsSep ", " (
              x:
              if isString x then
                x
              else if isInt x then
                toString x
              else if isStorePath x then
                toString x
              else if isBool x then
                if value then "yes" else "no"
              else
                abort "Unsupported type in Dovecot configuration attrset!"
            ) value
          else if isList value && name == "include'" then
            concatMapStringsSep "\n" (x: "include! " + x) value
          else if isList value && name == "include_try'" then
            concatMapStringsSep "\n" (x: "include_try! " + x) value
          else
            abort "Unsupported type in Dovecot configuration attrset!"
        ) cfg
      )
    else
      abort "Unsupported type in Dovecot configuration attrset!";

  postfix = {
    toMainCnf =
      cfg:
      if isAttrs cfg then
        concatStringsSep "\n" (
          mapAttrsToList (
            name: value:
            if isNull value then
              ""
            else if isString value then
              "${name} = ${value}"
            else if isInt value then
              "${name} = ${toString value}"
            else if isStorePath value then
              "${name} = ${toString value}"
            else if isBool value then
              if value then "${name} = yes" else "${name} = no"
            else if isList value then
              "${name} = "
              + concatMapStringsSep ", " (
                x:
                if isString x then
                  x
                else if isInt x then
                  toString x
                else if isStorePath x then
                  toString x
                else if isBool x then
                  if value then "yes" else "no"
                else
                  abort "Unsupported type in Postfix main configuration attrset!"
              ) value
            else
              abort "Unsupported type in Postfix main configuration attrset!"
          ) cfg
        )
      else
        abort "Unsupported type in Postfix main configuration attrset!";
  };

  php = {
    ini = cfg: concatStringsSep "\n" (mapAttrsToList (name: value: "${name} = ${toString value}") cfg);
    fpm =
      env: cfg: header:
      concatStringsSep "\n" (
        [ "[${header}]" ]
        ++ (mapAttrsToList (name: value: "${name} = ${toString value}") cfg)
        ++ (mapAttrsToList (name: value: "env[${name}] = ${toString value}") env)
        ++ [ "" ]
      );
  };
}
