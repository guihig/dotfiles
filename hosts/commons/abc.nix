{pkgs, ...}: {
  # Time Config
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
    networkmanager.enable = true;
  };

  # Enable sound
  sound.enable = true;

  # Env config
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

  # Extra
  xdg.portal.enable = true;
  virtualisation.docker.enable = true;

  # Create fonts dir
  fonts.fontDir.enable = true;
}
