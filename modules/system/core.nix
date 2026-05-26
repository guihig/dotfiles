{
  flake.modules.nixos.core = {pkgs, ...}: {
    # Env config
    environment = {
      variables = {
        SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS = 0;
        SHELL = "fish";
      };
      systemPackages = with pkgs; [
        firefox-bin
        git
        wget
        curl
        vim
        neovim
        home-manager
        polkit_gnome
        gnome-keyring
        file-roller
        kitty
        lxqt.lxqt-policykit
        clamav
        SDL2
      ];
    };

    # Extra
    virtualisation.docker.enable = true;

    # Create fonts dir
    fonts.fontDir.enable = true;

    programs = {
      nm-applet = {
        enable = true;
        indicator = false;
      };

      dconf.enable = true;

      thunar = {
        enable = true;

        plugins = with pkgs.xfce; [
          thunar-archive-plugin
          thunar-media-tags-plugin
          thunar-volman
        ];
      };

      openvpn3.enable = true;
    };
  };
}
