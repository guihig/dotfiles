{
  flake.modules.homeManager.tmuxinator = {pkgs, ...}: {
    home.packages = with pkgs; [unstable.tmuxinator];
    home.file.".config/tmuxinator" = {
      source = ./configs;
      recursive = true;
    };

    xdg.configFile."fish/completions/tmuxinator.fish".source = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/tmuxinator/tmuxinator/master/completion/tmuxinator.fish";
      hash = "sha256-hgLG69P+7VBp69bsWIHK+/wz1zJZingSFavUjQNirtU";
    };
  };
}
