{pkgs, ...}: {
  home.packages = with pkgs; [
    # very cool code kid0
    vim
    nano
    vscode
    cargo
    rustc
    go
    gcc
    gnumake
    lua
    nodejs
    tree-sitter
    luaPackages.luarocks
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
    jetbrains.pycharm-professional
    docker-compose
    postgresql_13
    insomnia
    code-minimap

    # Lsps
    lua-language-server
    # nodePackages.vscode-langservers-extracted
    # nodePackages.dockerfile-language-server-nodejs
    # nodePackages.typescript-language-server
    # nodePackages.yaml-language-server
    # nodePackages.pyright
    # nodePackages.volar
    texlab
    # sqls
    # efm-langserver
    # rnix-lsp
    # rust-analyzer
    # elixir-ls

    # Code formatters
    stylua
    yamlfmt
    black
    nodePackages.sql-formatter
    stylish-haskell
    alejandra
    texlive.combined.scheme-full
  ];
}
