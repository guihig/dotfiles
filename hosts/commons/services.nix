{pkgs, ...}: {
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

    # Audio with pipewire
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
