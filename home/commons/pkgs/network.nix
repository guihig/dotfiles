{pkgs, ...}: {
  home.packages = with pkgs; [
    openfortivpn
    networkmanagerapplet
  ];
}
