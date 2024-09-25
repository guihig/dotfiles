{pkgs, ...}: {
  home.packages = with pkgs; [
    texlab
  ];
}
