{pkgs, ...}: {
  # Programs
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

    ssh = {
      startAgent = true;
    };

    # Noise supression
    noisetorch.enable = true;
  };
}
