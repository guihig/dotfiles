{
  inputs,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    waybar
    wl-gammactl
    wl-clipboard
    wf-recorder
    wlprop
    hyprpicker
    wayshot
    swappy
    grim
    slurp

    imagemagick
    swww
    cliphist
  ];

  services.flameshot = {
    enable = true;
    settings = {
      General = {
        useGrimAdapter = true;
        disabledGrimWarning = true;
      };
    };
  };

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
          font_family = "$font_family";

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

  services.wpaperd = {
    enable = true;
    settings = {
      DP-3 = {
        path = "~/.config/wallpapers/forest_house.jpg";
      };
      DP-4 = {
        path = "~/.config/wallpapers/2024_Into_the_Light_Press_Kit_Cinematics_COMPRESSED_003.jpg";
      };
      DP-2 = {
        path = "~/.config/wallpapers/2024_Into_the_Light_Press_Kit_Cinematics_COMPRESSED_003.jpg";
      };
    };
  };

  home.sessionVariables = {
    NIXOS_OZONE_WL = 1;
  };

  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.plugins = [
    inputs.split-monitor-workspaces.packages.${pkgs.stdenv.hostPlatform.system}.split-monitor-workspaces
  ];
  wayland.windowManager.hyprland.settings = {
    "$monitor_left" = "DP-3";
    "$monitor_center" = "DP-4";
    "$monitor_top" = "DP-2";

    env = [
      # XDG
      "XDG_CURRENT_DESKTOP,Hyprland"
      "XDG_SESSION_TYPE,wayland"
      "XDG_SESSION_DESKTOP,Hyprland"

      # Toolkit
      "GDK_BACKEND,wayland,x11"
      "SDL_VIDEODRIVER,wayland"
      "CLUTTER_BACKEND,wayland"

      # QT
      "QT_AUTO_SCREEN_SCALE_FACTOR,1"
      "QT_QPA_PLATFORM,wayland;xcb"

      # Nvidia
      "GBM_BACKEND,nvidia-drm"
      "__GLX_VENDOR_LIBRARY_NAME,nvidia"
      "LIBVA_DRIVER_NAME,nvidia"

      # Electron
      "ELECTRON_OZONE_PLATFORM_HINT,auto"
    ];

    monitor = [
      "$monitor_left, 3440x1440@144, -3440x25, 1"
      "$monitor_center, 1920x1080@240, 0x0, 1"
      "$monitor_top, 1920x1080@60, 270x-1080, 1"
    ];

    exec-once = [
      "lxqt-policykit-agent"
      "caelestia shell -d"
      "vesktop"
      "spotify"
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

      "SUPER_SHIFT, h, movewindow, mon:l"
      "SUPER_SHIFT, l, movewindow, mon:r"
      "SUPER_SHIFT, k, movewindow, mon:u"
      "SUPER_SHIFT, j, movewindow, mon:d"

      # Switch workspaces with mainMod + [0-9]
      "SUPER, 1, split-workspace, 1"
      "SUPER, 2, split-workspace, 2"
      "SUPER, 3, split-workspace, 3"
      "SUPER, 4, split-workspace, 4"
      "SUPER, 5, split-workspace, 5"
      "SUPER, 6, split-workspace, 6"
      "SUPER, 7, split-workspace, 7"
      "SUPER, 8, split-workspace, 8"
      "SUPER, 9, split-workspace, 9"
      "SUPER, 0, split-workspace, 10"

      # Move active window to a workspace with mainMod + SHIFT + [0-9]
      "SUPER_SHIFT, 1, split-movetoworkspace, 1"
      "SUPER_SHIFT, 2, split-movetoworkspace, 2"
      "SUPER_SHIFT, 3, split-movetoworkspace, 3"
      "SUPER_SHIFT, 4, split-movetoworkspace, 4"
      "SUPER_SHIFT, 5, split-movetoworkspace, 5"
      "SUPER_SHIFT, 6, split-movetoworkspace, 6"
      "SUPER_SHIFT, 7, split-movetoworkspace, 7"
      "SUPER_SHIFT, 8, split-movetoworkspace, 8"
      "SUPER_SHIFT, 9, split-movetoworkspace, 9"
      "SUPER_SHIFT, 0, split-movetoworkspace, 10"

      "SUPER, A, togglespecialworkspace, vesktop"
      "SUPER, O, exec, vesktop"
      "SUPER, O, focusworkspaceoncurrentmonitor, special:vesktop"
      "SUPER, S, togglespecialworkspace, spotify"

      # Important
      "SUPER, Period, exec, caelestia emoji -p"
    ];

    bindm = [
      # Move/resize windows with mainMod + LMB/RMB and dragging
      "SUPER, mouse:272, movewindow"
      "SUPER, mouse:273, resizewindow"
    ];

    # workspace = [
    #   # Set stick workspaces to the monitors
    #   "1, monitor:$monitor_left"
    #   "3, monitor:$monitor_left"
    #
    #   "2, monitor:$monitor_center"
    #   "4, monitor:$monitor_center"
    #   "5, monitor:$monitor_center"
    #
    #   "6, monitor:$monitor_top"
    # ];

    general = {
      gaps_in = 6;
      gaps_out = 12;
      border_size = 2;

      allow_tearing = false;

      "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
      "col.inactive_border" = "rgba(595959aa)";

      layout = "dwindle";
    };

    cursor = {
      no_hardware_cursors = 0;
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
        "workspaces, 1, 7, fluent_decel, slide"
        "specialWorkspace, 1, 3, overshot, slidevert"
      ];
    };

    misc = {
      vfr = 1;
      vrr = 1;
      focus_on_activate = true;
      animate_manual_resizes = false;
      animate_mouse_windowdragging = false;
      enable_swallow = false;
      swallow_regex = "(foot|kitty|allacritty|Alacritty)";
      disable_hyprland_logo = true;
      force_default_wallpaper = 0;
    };

    windowrule = [
      # Dialogs
      "match:title (Select|Open)( a)? (File|Folder)(s)?, float on"
      "match:title File (Operation|Upload)( Progress)?, float on"
      "match:title .* Properties, float on"
      "match:title Save As, float on"
      "match:title Library, float on"
      "match:title ^(.*)(Extension:)(.*)(- Bitwarden)(.*)$, float on"

      # Picture in picture (resize and move done via script)
      "move 100%-w-2% 100%-w-3%, match:title Picture(-| )in(-| )[Pp]icture"
      "keep_aspect_ratio on, match:title Picture(-| )in(-| )[Pp]icture"
      "float on, match:title Picture(-| )in(-| )[Pp]icture"
      "pin on, match:title Picture(-| )in(-| )[Pp]icture"

      #------------- Games
      "opaque on, match:class (steam_app_(default|[0-9]+)|gamescope"
      "immediate on, match:class (steam_app_(default|[0-9]+)|gamescope"
      "idle_inhibit always, match:class (steam_app_(default|[0-9]+)|gamescope"
      "workspace 15 silent, match:class ^(steam|steamwebhelper|steam_app_.*|Steam|steam_app_(default|[0-9]+)|gamescope)$"
      "workspace 15 silent, match:title (World of Warcraft)"
      "fullscreen on, match:class ^(steam_app_(default|[0-9]+))$"
      "fullscreen on, match:title (World of Warcraft)"
      "float on, match:title ^(Steam - Self Updater)$"
      "float on, match:title Friends List, match:class steam"

      #------------- Special Workspaces
      # -- Spotify
      "workspace special:spotify silent, match:class ^(Spotify)$"
      "float on, match:class ^(Spotify)$"
      "no_anim on, match:class ^(Spotify)$"
      "size monitor_w*0.71 monitor_h*0.71, match:class ^(Spotify)$"
      "center on, match:class ^(Spotify)$"

      # -- Discord
      "workspace special:vesktop silent, match:class (vesktop|discord)"
      "float on, match:class (vesktop|discord)"
      "no_anim on, match:class (vesktop|discord)"
      "size monitor_w*0.71 monitor_h*0.71, match:class (vesktop|discord)"
      "center on, match:class (vesktop|discord)"
    ];
  };

  wayland.windowManager.hyprland.extraConfig = ''
    debug {
      disable_logs=false
    }

    plugin {
      split-monitor-workspaces {
        count = 5
      }
    }

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
