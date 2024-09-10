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
    inputs.sops-nix.nixosModules.sops
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
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

  # Bootloader.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    # systemd-boot.enable = true;
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
      useOSProber = true;
    };
  };
  boot.supportedFilesystems = ["ntfs"];

  # Set your time zone.
  time = {
    timeZone = "America/Sao_Paulo";
    hardwareClockInLocalTime = true;
  };

  # I18n Config
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "pt_BR.UTF-8";
      LC_IDENTIFICATION = "pt_BR.UTF-8";
      LC_MEASUREMENT = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
      LC_NAME = "pt_BR.UTF-8";
      LC_NUMERIC = "pt_BR.UTF-8";
      LC_PAPER = "pt_BR.UTF-8";
      LC_TELEPHONE = "pt_BR.UTF-8";
      LC_TIME = "pt_BR.UTF-8";
    };
  };

  # Networking
  networking = {
    hostName = "ferreira-pc";
    networkmanager.enable = true;
  };

  # Enable sound
  sound.enable = true;

  # Services
  services = {
    gnome.gnome-keyring.enable = true;
    flatpak.enable = true;
    davfs2.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    blueman.enable = true;
    spice-vdagentd.enable = true;
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
      };
    };

    greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          command = "${pkgs.hyprland}/bin/Hyprland";
          user = "ferreira";
        };
        default_session = initial_session;
      };
    };

    # XServer Config
    xserver.videoDrivers = ["nvidia"];

    # Audio with pipewire
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };

  # Programs
  programs = {
    hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    nm-applet = {
      enable = true;
      indicator = false;
    };

    fish.enable = true;

    dconf.enable = true;

    thunar = {
      enable = true;

      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-media-tags-plugin
        thunar-volman
      ];
    };

    slock.enable = true;

    xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.slock}/bin/slock";
    };

    ssh = {
      startAgent = true;
    };

    # Noise supression
    noisetorch.enable = true;
  };

  # Hardware
  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
    nvidia = {
      modesetting.enable = true;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
    bluetooth = {
      enable = true;
    };
    pulseaudio.enable = false;
    keyboard.qmk.enable = true;
  };

  security = {
    polkit.enable = true;
    pam = {
      services = {
        login.enableGnomeKeyring = true;
      };
    };
    rtkit.enable = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [
      firefox
      git
      wget
      curl
      vim
      neovim
      home-manager
      polkit_gnome
      gnome.gnome-keyring
      gnome.file-roller
      kitty
      lxqt.lxqt-policykit
    ];
    etc."ppp/options".text = "ipcp-accept-remote";
  };

  # Sops Config
  sops = {
    age.sshKeyPaths = ["/etc/ssh/id_ed25519"];
    age.keyFile = "/home/ferreira/.config/sops/age/keys.txt";
    defaultSopsFile = ../../secrets/common/secrets.yaml;

    secrets."passwd/ferreira" = {
      neededForUsers = true;
    };
  };

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

  # Extra
  xdg.portal.enable = true;
  virtualisation.docker.enable = true;

  # Kernel
  boot.kernelPackages = pkgs.unstable.linuxPackages_zen;

  # Create fonts dir
  fonts.fontDir.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
