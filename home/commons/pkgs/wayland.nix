{pkgs, ...}: {
  home.packages = with pkgs; [
    waybar
    wl-gammactl
    wl-clipboard
    wf-recorder
    wlprop
    hyprpicker
    wayshot
    swappy
    grim
    slurp
    imagemagick
    swww
    cliphist
    xwaylandvideobridge
    xdg-desktop-portal-hyprland
  ];
}
