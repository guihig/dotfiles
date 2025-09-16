{pkgs, ...}: {
  home.packages = with pkgs; [
    unstable.elixir-ls
    unstable.emmet-ls
    unstable.nodePackages.bash-language-server
    unstable.nodePackages.typescript-language-server
    unstable.dockerfile-language-server
    unstable.vscode-langservers-extracted
    unstable.vue-language-server
    unstable.nil
    unstable.lua-language-server
    unstable.vtsls
    texlab
  ];

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    package = pkgs.unstable.neovim-unwrapped;
  };

  home.file.".config/nvim" = {
    source = ../../config/nvim;
    recursive = true;
  };

  home.file.".config/nvim/lua/lsp_location.lua" = with pkgs; {
    text = ''
      return {
        bashls = { "${unstable.nodePackages.bash-language-server}/bin/bash-language-server", "start" },
        dockerls = { "${unstable.dockerfile-language-server}/bin/docker-langserver", "--stdio" },
        elixirls = { "${unstable.elixir-ls}/bin/elixir-ls" },
        eslint = { "${unstable.vscode-langservers-extracted}/bin/vscode-eslint-language-server", "--stdio" },
        html = { "${unstable.vscode-langservers-extracted}/bin/vscode-html-language-server", "--stdio" },
        jsonls = { "${unstable.vscode-langservers-extracted}/bin/vscode-json-language-server", "--stdio" },
        cssls = { "${unstable.vscode-langservers-extracted}/bin/vscode-css-language-server", "--stdio" },
        ts_ls = { "${unstable.nodePackages.typescript-language-server}/bin/typescript-language-server", "--stdio" },
        vue_ls = { "${unstable.vue-language-server}/bin/vue-language-server", "--stdio" },
        nil_ls = { "${unstable.nil}/bin/nil" },
        lua_ls = { "${unstable.lua-language-server}/bin/lua-language-server" },
        emmet_ls = { "${unstable.emmet-ls}/bin/emmet-ls", "--stdio" },
        expert = { "/home/ferreira/dev/misc/expert/apps/expert/burrito_out/expert_linux_amd64" },
        vue_ts_plugin = "${unstable.vue-language-server}/lib/node_modules/@vue/language-server",
        vtsls = { "${unstable.vtsls}/bin/vtsls", "--stdio" },
        awesomewm_lib = "${pkgs.awesome-git}/share/awesome/lib"
      }
    '';
  };
}
