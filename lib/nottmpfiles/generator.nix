{ lib, nglib, ... }:
rules:
let
  fromNull = x: if x == null then "-" else x;
in
(lib.concatMapStringsSep "\n" (
  {
    type,
    path,
    mode,
    user,
    group,
    age,
    argument,
  }:
  "${fromNull type} ${fromNull path} ${fromNull mode} ${fromNull user} ${fromNull group} ${fromNull age} ${fromNull argument}"
) rules)
+ "\n"
