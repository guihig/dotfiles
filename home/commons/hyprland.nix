{pkgs, ...}: {
  home.file.".config/waybar" = {
    source = ../../config/waybar;
    recursive = true;
  };

  home.file.".config/wallpapers" = {
    source = ../../wallpapers;
    recursive = true;
  };

  programs.hyprlock = {
    enable = true;
    settings = {
      "$text_color" = "rgba(FFFFFFFF)";
      "$entry_background_color" = "rgba(33333311)";
      "$entry_border_color" = "rgba(3B3B3B55)";
      "$entry_color" = "rgba(FFFFFFFF)";
      "$font_family" = "Jetbrains Mono";
      "$font_family_clock" = "Jetbrains Mono";

      general = {
        disable_loading_bar = true;
        grace = 300;
        hide_cursor = true;
        no_fade_in = false;
      };

      background = [
        {
          color = "rgba(000000FF)";
        }
      ];

      input-field = [
        {
          monitor = "";
          size = "250, 50";
          outline_thickness = 2;
          dots_size = 0.1;
          dots_spacing = 0.3;
          outer_color = "$entry_border_color";
          inner_color = "$entry_background_color";
          font_color = "$entry_color";
          position = "0, 20";
          halign = "center";
          valign = "center";
        }
      ];

      label = [
        {
          monitor = "";
          text = "$TIME";
          shadow_passes = "1";
          shadow_boost = "0.5";
          color = "$text_color";
          font_size = "65";
          font_family = "$font_family_clock";

          position = "0, 300";
          halign = "center";
          valign = "center";
        }
        {
          monitor = "";
          text = "locked";
          shadow_passes = "1";
          shadow_boost = "0.5";
          color = "$text_color";
          font_size = "14";
          font_family = "$font_family";
          position = "0, 45";
          halign = "center";
          valign = "bottom";
        }
      ];
    };
  };

  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.package = pkgs.unstable.hyprland;
  wayland.windowManager.hyprland.settings = {
    "$monitor_left" = "DP-2";
    "$monitor_center" = "DP-3";
    "$monitor_right" = "DP-1";

    env = [
      "XDG_SESSION_TYPE,wayland"
      "GBM_BACKEND,nvidia-drm"
      "__GLX_VENDOR_LIBRARY_NAME,nvidia"
      "WLR_NO_HARDWARE_CURSORS,1"
    ];

    monitor = [
      "$monitor_left, 3440x1440@144, -3440x25, 1"
      "$monitor_center, 1920x1080@240, 0x0, 1"
      "$monitor_right, 1920x1080@60, 270x-1080, 1"
    ];

    exec-once = [
      "swww init"
      "lxqt-policykit-agent"
      "swww img ~/.config/wallpapers/forest_house.jpg --transition-type=grow -o DP-2"
      "swww img ~/.config/wallpapers/2024_Into_the_Light_Press_Kit_Cinematics_COMPRESSED_003.jpg --transition-type=grow"
      "waybar"
      "vesktop"
      "spotify"
      "wl-paste --type text --watch cliphist store # Stores only text data"
      "wl-paste --type image --watch cliphist store # Stores only image data"
    ];

    input = {
      kb_layout = "us";
      kb_variant = "intl";
      follow_mouse = 1;
      mouse_refocus = true;
    };

    bind = [
      # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
      "SUPER, Return, exec, kitty"
      "SUPER, w, killactive"
      "SUPER_SHIFT, w, exec, hyprctl kill"
      "SUPER, Space, exec, rofi -show drun"

      "SUPER, F, fullscreen, 1"
      "SUPER, T, togglegroup, "
      "SUPER, Y, togglefloating, "
      ", Print, exec, grim -g \"$(slurp -d)\" - | wl-copy"
      ", Scroll_Lock, exec, hyprlock --immediate"

      # Sound control
      ", XF86AudioRaiseVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ +2%"
      ", XF86AudioLowerVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ -2%"
      ", XF86AudioMute, exec, pactl set-sink-mute @DEFAULT_SINK@ toggle"
      ", XF86AudioMicMute, exec, pactl set-source-mute @DEFAULT_SOURCE@ toggle"

      # Window movement
      "SUPER, h, movefocus, l"
      "SUPER, l, movefocus, r"
      "SUPER, k, movefocus, u"
      "SUPER, j, movefocus, d"

      "SUPER_CTRL, h, swapwindow, l"
      "SUPER_CTRL, l, swapwindow, r"
      "SUPER_CTRL, k, swapwindow, u"
      "SUPER_CTRL, j, swapwindow, d"

      "SUPER_SHIFT, h, movewindow, l"
      "SUPER_SHIFT, l, movewindow, r"
      "SUPER_SHIFT, k, movewindow, u"
      "SUPER_SHIFT, j, movewindow, d"

      # Switch workspaces with mainMod + [0-9]
      "SUPER, 1, workspace, 1"
      "SUPER, 2, workspace, 2"
      "SUPER, 3, workspace, 3"
      "SUPER, 4, workspace, 4"
      "SUPER, 5, workspace, 5"
      "SUPER, 6, workspace, 6"
      "SUPER, 7, workspace, 7"
      "SUPER, 8, workspace, 8"
      "SUPER, 9, workspace, 9"
      "SUPER, 0, workspace, 10"

      # Move active window to a workspace with mainMod + SHIFT + [0-9]
      "SUPER_SHIFT, 1, movetoworkspace, 1"
      "SUPER_SHIFT, 2, movetoworkspace, 2"
      "SUPER_SHIFT, 3, movetoworkspace, 3"
      "SUPER_SHIFT, 4, movetoworkspace, 4"
      "SUPER_SHIFT, 5, movetoworkspace, 5"
      "SUPER_SHIFT, 6, movetoworkspace, 6"
      "SUPER_SHIFT, 7, movetoworkspace, 7"
      "SUPER_SHIFT, 8, movetoworkspace, 8"
      "SUPER_SHIFT, 9, movetoworkspace, 9"
      "SUPER_SHIFT, 0, movetoworkspace, 10"

      "SUPER, A, togglespecialworkspace, discord"
      "SUPER, O, exec, vesktop"
      "SUPER, O, focusworkspaceoncurrentmonitor, special:discord"
      "SUPER, S, togglespecialworkspace, spotify"
    ];

    bindm = [
      # Move/resize windows with mainMod + LMB/RMB and dragging
      "SUPER, mouse:272, movewindow"
      "SUPER, mouse:273, resizewindow"
    ];

    workspace = [
      # Set stick workspaces to the monitors
      "1, monitor:$monitor_left"
      "3, monitor:$monitor_left"

      "2, monitor:$monitor_center"
      "4, monitor:$monitor_center"

      "6, monitor:$monitor_right"
    ];

    general = {
      gaps_in = 6;
      gaps_out = 12;
      border_size = 2;

      "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
      "col.inactive_border" = "rgba(595959aa)";

      layout = "dwindle";
    };

    decoration = {
      rounding = 10;
      active_opacity = 1;
      inactive_opacity = 0.85;

      blur = {
        enabled = true;
        xray = true;
        special = false;
        new_optimizations = "on";
        size = 8;
        passes = 4;
        brightness = 1;
        noise = 0.01;
        contrast = 1;
      };

      dim_inactive = false;
      dim_strength = 0.1;
      dim_special = 0.3;
    };

    animations = {
      enabled = "yes";
      bezier = [
        "linear, 0, 0, 1, 1"
        "md3_standard, 0.2, 0, 0, 1"
        "md3_decel, 0.05, 0.7, 0.1, 1"
        "md3_accel, 0.3, 0, 0.8, 0.15"
        "overshot, 0.05, 0.9, 0.1, 1.1"
        "crazyshot, 0.1, 1.5, 0.76, 0.92 "
        "hyprnostretch, 0.05, 0.9, 0.1, 1.0"
        "fluent_decel, 0.1, 1, 0, 1"
        "easeInOutCirc, 0.85, 0, 0.15, 1"
        "easeOutCirc, 0, 0.55, 0.45, 1"
        "easeOutExpo, 0.16, 1, 0.3, 1"
      ];
      # Animation configs
      animation = [
        "windows, 1, 3, md3_decel, popin 60%"
        "border, 1, 10, default"
        "borderangle, 1, 500, linear, loop"
        "fade, 1, 2.5, md3_decel"
        # "workspaces, 1, 3.5, md3_decel, slide"
        "workspaces, 1, 7, fluent_decel, slide"
        # "workspaces, 1, 7, fluent_decel, slidefade 15%"
        # "specialWorkspace, 1, 3, md3_decel, slidefadevert 15%"
        "specialWorkspace, 1, 3, overshot, slidevert"
      ];
    };

    misc = {
      vfr = 1;
      vrr = 1;
      # layers_hog_mouse_focus = true;
      focus_on_activate = true;
      animate_manual_resizes = false;
      animate_mouse_windowdragging = false;
      enable_swallow = false;
      swallow_regex = "(foot|kitty|allacritty|Alacritty)";

      disable_hyprland_logo = true;
      force_default_wallpaper = 0;
      new_window_takes_over_fullscreen = 2;
    };

    windowrule = [
      # "noblur,.*"

      # Dialogs
      "float,title:^(Open File)(.*)$"
      "float,title:^(Select a File)(.*)$"
      "float,title:^(Choose wallpaper)(.*)$"
      "float,title:^(Open Folder)(.*)$"
      "float,title:^(Save As)(.*)$"
      "float,title:^(Library)(.*)$"
    ];

    windowrulev2 = [
      "opacity 1 override 1 override,class:^(vesktop)$"
      "opacity 1 override 1 override,class:^(firefox)$"
      "float,title:^(.*)(Extension:)(.*)(- Bitwarden)(.*)$"

      "workspace special:spotify silent,class:^(Spotify)$"
      "float,class:^(Spotify)$"
      "noanim,class:^(Spotify)$"
      "size 71% 71%,class:^(Spotify)$"
      "center 1,class:^(Spotify)$"

      "workspace special:discord silent,class:^(vesktop)$"
      "float,class:^(vesktop)$"
      "noanim,class:^(vesktop)$"
      "size 71% 71%,class:^(vesktop)$"
      "move 12 14.5%,class:^(vesktop)$"
    ];
  };

  wayland.windowManager.hyprland.extraConfig = ''
    # window resize
    bind = SUPER, R, submap, resize

    submap = resize
    binde = , l, resizeactive, 30 0
    binde = , h, resizeactive, -30 0
    binde = , k, resizeactive, 0 -30
    binde = , j, resizeactive, 0 30
    bind = , escape, submap, reset
    submap = reset
  '';
}
