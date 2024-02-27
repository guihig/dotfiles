{
  # ---- Picom Config ---- #
  services.picom = {
    enable = true;
    fade = true;
    fadeSteps = [0.1 0.1];
    shadow = true;
    shadowOffsets = [(-2) (-2)];
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = ''"
      "_GTK_FRAME_EXTENTS@:c"
    ];
    inactiveOpacity = 0.9;
    wintypes = {
      tooltip = {
        fade = true;
        shadow = true;
        opacity = 0.75;
        focus = true;
        full-shadow = false;
      };
      dock = {
        shadow = false;
        clip-shadow-above = true;
      };
      dnd = {shadow = false;};
      popup_menu = {
        shadow = false;
        fade = false;
      };
    };
    backend = "glx";
    opacityRules = [
      "100:name = 'Picture-in-Picture'"
      "100:fullscreen"
    ];
    settings = {
      transparent-clipping = false;
      inactive-opacity-override = false;
      frame-opacity = 1.0;
      rounded-corners-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
      ];
      blur-kern = "3x3box";
      blur-background-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
        "_GTK_FRAME_EXTENTS@:c"
      ];
      glx-no-stencil = true;
      shadow-radius = 7;
    };
  };
}
