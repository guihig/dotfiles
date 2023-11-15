{
  inputs,
  pkgs,
  ...
}: {
  # ---- Awesome Configuration ---- #
  xsession.windowManager.awesome = {
    enable = true;
  };
  home.file.".config/wallpapers" = {
    source = ../../wallpapers;
    recursive = true;
  };
  home.file.".config/awesome" = {
    source = ../../config/awesome;
    recursive = true;
  };
  home.file.".config/awesome/modules/bling" = {
    source = inputs.bling.outPath;
    recursive = true;
  };
  home.file.".config/awesome/modules/rubato" = {
    source = inputs.rubato.outPath;
    recursive = true;
  };
  home.file.".config/awesome/lain" = {
    source = inputs.lain.outPath;
    recursive = true;
  };
}
