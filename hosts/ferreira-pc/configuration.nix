# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # You can import other NixOS modules here
  imports = [
    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix
    ../commons/abc.nix
    ../commons/boot.nix
    ../commons/security.nix
    ../commons/hardware.nix
    ../commons/sops.nix
    ../commons/programs.nix
    ../commons/services.nix
    ../commons/wayland.nix
    inputs.sops-nix.nixosModules.sops
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      packageOverrides = pkgs: {
        vaapiIntel = pkgs.vaapiIntel.override {enableHybridCodec = true;};
      };
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flakeconf.nix
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
    };
  };

  networking.hostName = "ferreira-pc";
  services.xserver.videoDrivers = ["nvidia" "intel"];

  users.users = {
    ferreira = {
      isNormalUser = true;
      hashedPasswordFile = config.sops.secrets."passwd/ferreira".path;
      shell = pkgs.fish;
      description = "CABECA";
      extraGroups = ["networkmanager" "wheel" "docker"];
      packages = with pkgs; [
        firefox
        git
        wget
        curl
        vim
        neovim
      ];
    };
  };

  # Kernel
  boot.kernelPackages = pkgs.unstable.linuxPackages_zen;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
