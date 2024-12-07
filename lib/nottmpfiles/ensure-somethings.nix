{ nglib, lib, ... }:
ensureSomethings:
with nglib.nottmpfiles.dsl;
(
  let
    link = lib.pipe ensureSomethings.link [
      (lib.mapAttrsToList (
        u:
        {
          src,
          dst,
          persistent,
        }:
        [ (Lp dst _ _ _ _ src) ] ++ lib.optional (!persistent) (r dst _ _ _ _ _)
      ))
      lib.flatten
    ];
    copy = lib.pipe ensureSomethings.copy [
      (lib.mapAttrsToList (
        u:
        {
          src,
          dst,
          persistent,
        }:
        [ (Cp src _ _ _ _ dst) ] ++ lib.optional (!persistent) (r src _ _ _ _ _)
      ))
      lib.flatten
    ];
    create = lib.pipe ensureSomethings.create [
      (lib.mapAttrsToList (
        u:
        {
          dst,
          type,
          mode,
          owner,
          persistent,
        }:
        let
          ownerParts = lib.splitString ":" owner;
          user = builtins.elemAt ownerParts 0;
          group = builtins.elemAt ownerParts 1;
          mode' = if builtins.stringLength mode == 3 then "0${mode}" else mode;

          do.file = (f dst mode' user group _ _);
          do.directory = (d dst mode' user group _ _);
          remove.file = (r dst _ _ _ _ _);
          remove.directory = (R dst _ _ _ _ _);
        in
        assert (
          lib.assertMsg (
            lib.length ownerParts == 2
          ) "the owner field must be of the form `<user>:<group>`, but is `${owner}`"
        );
        [ do.${type} ] ++ lib.optional (!persistent) [ remove.${type} ]
      ))
      lib.flatten
    ];
  in
  link ++ copy ++ create
)
