{ lib, nglib, ... }:
{
  mkUserOption =
    user: description:
    lib.mkOption {
      inherit description;
      type = lib.types.str;
      default = user;
    };

  mkGroupOption =
    group: description:
    lib.mkOption {
      inherit description;
      type = lib.types.str;
      default = group;
    };

  getOptionFromPath =
    path: options:
    let
      getOptionFromPath' =
        pathLeft: pathRight: subtree:
        if pathRight == [ ] then
          subtree
        else
          let
            newSubtree = (
              subtree.${lib.head pathRight} or (lib.evalModules {
                modules = subtree.type.getSubModules ++ subtree.definitions;
                inherit (subtree.type.functor.payload) class specialArgs;
              }).options.${lib.head pathRight}
              or (abort ("cannot find option '" + lib.concatStringsSep "." path + "'"))
            );
          in
          getOptionFromPath' (pathLeft ++ [ (lib.head pathRight) ]) (lib.tail pathRight) newSubtree;
    in
    getOptionFromPath' [ ] path options;

  mkOptionsEqual =
    to: from:
    { config, options, ... }:
    let
      fromOpt = nglib.getOptionFromPath from options;
      toOpt = nglib.getOptionFromPath to options;

      prio = fromOpt.highestPrio or lib.defaultOverridePriority;
      defsWithPrio = map (lib.mkOverride prio) fromOpt.definitions;
    in
    {
      config = lib.attrsets.setAttrByPath to (
        lib.mkMerge defsWithPrio
      );
      options = lib.attrsets.setAttrByPath from (
        lib.mkOption { apply = x: lib.attrsets.getAttrFromPath to config; }
      );
    };
}
