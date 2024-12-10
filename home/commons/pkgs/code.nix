{pkgs, ...}: {
  home.packages = with pkgs; [
    # Essential
    cargo
    rustc
    go
    gcc
    gnumake
    lua51Packages.lua
    lua51Packages.luarocks
    nodejs
    tree-sitter
    (let
      python3-with-packages = pkgs.python3.withPackages (p:
        with p; [
          pynvim
          setuptools
          pip
          psycopg2
          ds4drv
        ]);
    in
      python3-with-packages)
    docker-compose
    postgresql_13
    insomnia
    code-minimap
    zrok

    # Editors
    vim
    nano
    vscode
    jetbrains.pycharm-professional
    github-desktop
  ];
}
