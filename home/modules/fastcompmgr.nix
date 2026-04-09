{pkgs, ...}: {
  home.packages = with pkgs; [
    fastcompmgr
  ];

  systemd.user.services.fastcompmgr = {
    Unit = {
      Description = "FastCompMgr X11 Compositor";
      After = ["graphical-session.target"];
      PartOf = ["graphical-session.target"];
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "${pkgs.fastcompmgr}/bin/fastcompmgr -o 0.75 -r 7 -c -C -i 0.9";
    };
  };
}
