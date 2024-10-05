{nglib, lib, ... }:
ensureSomethings:
with nglib.tmpfiles.dsl;
(
let
  link = lib.pipe ensureSomethings.link [
    (lib.mapAttrsToList
      (u: { src, dst, persistent }: [
        (Lp dst    _     _ _ _ src)
      ] ++ lib.optional (!persistent)
        (r  src    _     _ _ _ _)))
    lib.flatten
  ]
   ;
 copy =
   lib.pipe ensureSomethings.copy [
     (lib.mapAttrsToList
       (u: { src, dst, persistent }: [
         (Cp src    _     _ _ _ dst)
       ] ++ lib.optional (!persistent)
         (r  src    _     _ _ _ _)))
     lib.flatten
   ];
 create = lib.pipe ensureSomethings.create [
   (lib.mapAttrsToList
     (u: { src, dst, type, mode, owner, persistent }:
       let
         do.file = (f  src mode owner _ _   _);
         do.directory = (d  src mode owner _ _   _);
         remove.file = (r _  _ _ _  _ _);
         remove.directory = (R _  _ _ _  _ _);
       in
         [
           do.${type}
         ]
       ++ lib.optional (!persistent)
           [
             remove.${type}
           ]))
   lib.flatten
 ];
in
  link ++ copy ++ create)
