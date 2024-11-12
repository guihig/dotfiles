{pkgs, ...}: {
  home.packages = with pkgs; [
    # Audio
    pavucontrol
    alsa-utils
    vlc

    # Codecs
    ffmpeg
  ];
}
