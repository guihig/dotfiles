{
  inputs,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    unstable.elixir-ls
    unstable.emmet-ls
    unstable.nodePackages.bash-language-server
    unstable.nodePackages.typescript-language-server
    unstable.dockerfile-language-server-nodejs
    unstable.vscode-langservers-extracted
    unstable.vue-language-server
    unstable.nil
    unstable.lua-language-server
    texlab
  ];

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
  };

  home.file.".config/nvim" = {
    source = ../../config/nvim;
    recursive = true;
  };

  home.file.".config/nvim/lua/lsp_location.lua" = with pkgs; {
    text = ''
      return {
        bashls = { "${unstable.nodePackages.bash-language-server}/bin/bash-language-server", "start" },
        dockerls = { "${unstable.dockerfile-language-server-nodejs}/bin/docker-langserver", "--stdio" },
        elixirls = { "${unstable.elixir-ls}/bin/elixir-ls" },
        eslint = { "${unstable.vscode-langservers-extracted}/bin/vscode-eslint-language-server", "--stdio" },
        html = { "${unstable.vscode-langservers-extracted}/bin/vscode-html-language-server", "--stdio" },
        jsonls = { "${unstable.vscode-langservers-extracted}/bin/vscode-json-language-server", "--stdio" },
        cssls = { "${unstable.vscode-langservers-extracted}/bin/vscode-css-language-server", "--stdio" },
        ts_ls = { "${unstable.nodePackages.typescript-language-server}/bin/typescript-language-server", "--stdio" },
        volar = { "${unstable.vue-language-server}/bin/vue-language-server", "--stdio" },
        nil_ls = { "${unstable.nil}/bin/nil" },
        lua_ls = { "${unstable.lua-language-server}/bin/lua-language-server" },
        emmet_ls = { "${unstable.emmet-ls}/bin/emmet-ls", "--stdio" },
        vue_ts_plugin = "${unstable.vue-language-server}/lib/node_modules/@vue/language-server",
        awesomewm_lib = "${pkgs.awesome-git}/share/awesome/lib"
      }
    '';
  };
}
