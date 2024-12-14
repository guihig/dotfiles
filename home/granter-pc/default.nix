{
  outputs,
  lib,
  config,
  ...
}: {
  imports = [
    outputs.homeManagerModules.helpers
    ../commons/pkgs/default.nix
    ../commons/pkgs/xorg.nix
    ../commons/picom.nix
    ../commons/awesome.nix
    ../commons/ui.nix
    ../commons/git.nix
    ../commons/ssh.nix
    ../commons/fish.nix
    ../commons/rofi.nix
    ../commons/terminal.nix
    ../commons/nvim.nix
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

    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = lib.mkDefault "24.05";
  };

  # Enable home-manager
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
