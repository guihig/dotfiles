{
  outputs,
  lib,
  config,
  ...
}: {
  imports = [
    outputs.homeManagerModules.helpers
    ../modules/packages.nix
    ../modules/hyprland.nix
    ../modules/caelestia.nix
    ../modules/dunst.nix
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
      permittedInsecurePackages = ["ventoy-1.1.07" "mbedtls-2.28.10" "qtwebengine-5.15.19"];
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
    stateVersion = lib.mkDefault "25.11";
  };

  programs.home-manager.enable = true;

  systemd.user.startServices = "sd-switch";
}
