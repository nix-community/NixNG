{ ... }:
{
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;

  settings.formatter.nixfmt = {
    excludes = [ ];
    includes = [ "*.nix" ];
    options = [ ];
  };
}
