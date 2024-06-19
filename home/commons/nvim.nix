{
  config,
  pkgs,
  ...
}: {
  programs.neovim = {
    enable = true;
    package = pkgs.unstable.neovim-unwrapped;
  };
  home.file.".config/nvim" = {
    source = config.lib.meta.mkMutableSymlink ../../config/nvim;
  };
}
