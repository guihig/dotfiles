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

  networking.hostName = "granter-pc";
  networking.networkmanager.enable = true;

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
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
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

  services.gnome.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  security.pam.services.login.enableGnomeKeyring = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = ["nvidia"];
    displayManager = {
      autoLogin.user = "ferreira";
      lightdm = {
        enable = true;
        greeters.gtk.enable = true;
      };
      defaultSession = "none+awesome";
    };
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome-git;
      luaModules = with pkgs.luaPackages; [luarocks lua-cjson inspect];
    };
    layout = "us";
    xkbVariant = "intl";
    exportConfiguration = true;
  };

  boot.kernelPackages = pkgs.linuxPackages_6_5;

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  hardware.nvidia = {
    modesetting.enable = true;
    # powerManagement.enable = false;
    # powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  hardware.bluetooth = {
    enable = true;
  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  hardware.keyboard.qmk.enable = true;
  environment.etc."ppp/options".text = "ipcp-accept-remote";
  environment.extraInit = ''
    xset s off -dpms
  '';

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
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
  ];

  sops = {
    age.sshKeyPaths = ["/etc/ssh/id_ed25519"];
    defaultSopsFile = ../../secrets/common/secrets.yaml;

    secrets."passwd/ferreira" = {
      neededForUsers = true;
    };
  };

  users.users = {
    ferreira = {
      isNormalUser = true;
      passwordFile = config.sops.secrets."passwd/ferreira".path;
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

  services.davfs2.enable = true;
  services.gvfs.enable = true;
  services.tumbler.enable = true;
  services.blueman.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
    };
  };

  services.spice-vdagentd.enable = true;
  virtualisation.docker.enable = true;

  programs = {
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
  };

  # Create fonts dir
  fonts.fontDir.enable = true;

  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = ["graphical-session.target"];
      wants = ["graphical-session.target"];
      after = ["graphical-session.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
