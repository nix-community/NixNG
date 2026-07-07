{ ... }:
{
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;
  programs.deadnix.enable = true;

  settings.formatter.nixfmt = {
    excludes = [ ];
    includes = [ "*.nix" ];
    options = [ ];
  };

  settings.formatter.deadnix = {
    excludes = [ ];
    includes = [ "*.nix" ];
    options = [ ];
  };
}
