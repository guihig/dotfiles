{
  # ---- Git Configuration ---- #
  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user = {
        name = "guihig";
        email = "guiih.ig@gmail.com";
      };
      alias = {
        s = "status";
        a = "add";
        p = "push";
        c = "commit";
      };
    };
  };
}
