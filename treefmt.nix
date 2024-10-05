{ ... }:
{
  projectRootFile = "flake.nix";
  programs.nixfmt-rfc-style.enable = true;

  settings.formatter.nixfmt-rfc-style = {
    excludes = [ ];
    includes = [ "*.nix" ];
    options = [ ];
  };
}
