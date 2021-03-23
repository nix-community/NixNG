{ lib, ... }:
with lib;
let
  cfg = config.assertions;
in
{
  options.assertions = mkOption {
    description = "List of assertions";
    type = types.listOf types.unspecified;
    default = [];
    example = [
      {
        assertion = 1 == 2;
        message = "The universe broke!";
      }
    ];
  };
}
