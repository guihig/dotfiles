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

      wireplumber.configPackages = [
        (pkgs.writeTextDir "share/wireplumber/main.lua.d/99-stop-microphone-auto-adjust.lua" ''
          table.insert (default_access.rules,{
              matches = {
                  {
                      { "application.process.binary", "=", "vesktop" }
                  }
              },
              default_permissions = "rx",
          })
        '')
      ];
    };
  };
}
