{
  # ---- Git Configuration ---- #
  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "guihig";
    userEmail = "guiih.ig@gmail.com";
    aliases = {
      s = "status";
      a = "add";
      p = "push";
      c = "commit";
    };
  };
}
