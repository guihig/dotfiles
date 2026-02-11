{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    unstable.picom
  ];

  xdg.configFile."picom/picom.conf".source = ../../../config/picom/picom.conf;

  systemd.user.services.picom = {
    Unit = {
      Description = "Picom X11 Compositor";
      After = ["graphical-session.target"];
      PartOf = ["graphical-session.target"];
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "${pkgs.unstable.picom}/bin/picom --config ${config.xdg.configFile."picom/picom.conf".source}";
    };
  };
}
