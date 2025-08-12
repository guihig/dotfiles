{pkgs, ...}: {
  home.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    nerd-fonts.terminess-ttf
    nerd-fonts.hack

    iosevka
    roboto
    iosevka-bin
    siji
    termsyn
    jetbrains-mono
    material-icons
    material-design-icons
    terminus_font
    font-awesome
    noto-fonts
    liberation_ttf
  ];
}
