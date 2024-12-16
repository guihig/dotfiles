{pkgs, ...}: {
  imports = [
    ./system.nix
    ./programs.nix
    ./code.nix
    ./formatters.nix
  ];
}
