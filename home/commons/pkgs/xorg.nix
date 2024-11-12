{pkgs, ...}: {
  home.packages = with pkgs; [
    xcowsay
    xclip
    arandr
  ];
}
