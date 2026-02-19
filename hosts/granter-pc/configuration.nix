{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../commons/abc.nix
    ../commons/boot.nix
    ../commons/security.nix
    ../commons/hardware.nix
    ../commons/nvidia.nix
    ../commons/sops.nix
    ../commons/programs.nix
    ../commons/services.nix
    ../../modules/nixos/awesome.nix
    inputs.sops-nix.nixosModules.sops
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };

  networking.hostName = "granter-pc";

  services.xserver.videoDrivers = ["modesetting"];
  hardware = {
    graphics = {
      extraPackages = with pkgs; [
        intel-media-sdk
      ];
    };
    nvidia = {
      prime = {
        offload = {
          enable = lib.mkForce false;
        };
        sync.enable = true;
        nvidiaBusId = "PCI:2:0:0";
        intelBusId = "PCI:0:2:0";
      };
    };
  };
  services.displayManager = {
    autoLogin.user = "ferreira";
  };

  users.users = {
    ferreira = {
      isNormalUser = true;
      hashedPasswordFile = config.sops.secrets."passwd/ferreira".path;
      shell = pkgs.fish;
      description = "CABECA";
      extraGroups = ["networkmanager" "wheel" "docker"];
      packages = with pkgs; [
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
  system.stateVersion = "25.11";
}
