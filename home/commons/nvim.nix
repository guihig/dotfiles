{config, ...}: {
  programs.neovim.enable = true;
  home.file.".config/nvim" = {
    source = config.lib.meta.mkMutableSymlink ../../config/nvim;
  };
}
