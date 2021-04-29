{ lib, ... }:
with lib;
{
  options.ids = {
    uids = mkOption {
      description = "A username to uid map, used for keeping track of assigned uids.";
      type = with types; attrsOf int;
      default = {
        root = 0;
        www-data = 54;
        hydra = 122;
        hydra-queue-runner = 235;
        hydra-www = 236;
        gitea = 399; # might change!
        nobody = 65534;
      };
    };
    gids = mkOption {
      description = "A groupname to gid map, used for keeping track of assigned gids.";
      type = with types; attrsOf int;
      default = {
        root = 0;
        www-data = 54;
        hydra = 122;
        hydra-queue-runner = 235;
        hydra-www = 236;
        log = 399;
        nogroup = 65534;
      };
    };
  };
}
