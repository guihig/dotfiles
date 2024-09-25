{pkgs, ...}: {
  home.packages = with pkgs; [
    # Code formatters
    stylua
    yamlfmt
    black
    nodePackages.sql-formatter
    nodePackages.prettier
    stylish-haskell
    alejandra
    texlive.combined.scheme-full
  ];
}
