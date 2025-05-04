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
    primaryPath: secondaryPath:
    { config, options, ... }:
    let
      secondaryOption = nglib.getOptionFromPath secondaryPath options;
      primaryOption = nglib.getOptionFromPath primaryPath options;

      prio = secondaryOption.highestPrio or lib.defaultOverridePriority;
      defsWithPrio = map (lib.mkOverride prio) secondaryOption.definitions;
    in
    {
      config = lib.attrsets.setAttrByPath primaryPath (
       lib.mkMerge defsWithPrio
      );
      options = lib.attrsets.setAttrByPath secondaryPath (
        lib.mkOption { apply = x: lib.attrsets.getAttrFromPath primaryPath config; }
      );
    };
}
