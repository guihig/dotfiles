{
  outputs,
  lib,
  config,
  ...
}: {
  imports = [
    outputs.homeManagerModules.helpers
    ../modules/packages.nix
    ../modules/awesome.nix
    ../modules/ui.nix
    ../modules/git.nix
    ../modules/ssh.nix
    ../modules/fish.nix
    ../modules/rofi.nix
    ../modules/terminal.nix
    ../modules/nvim.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = lib.mkDefault "ferreira";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    sessionPath = ["$HOME/.local/bin"];
    sessionVariables = {
      TERMINAL = "kitty";
      EDITOR = "nvim";
      BROWSER = "firefox";
    };

    stateVersion = lib.mkDefault "25.11";
  };

  programs.home-manager.enable = true;

  systemd.user.startServices = "sd-switch";
}
