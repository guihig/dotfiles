{self, ...}: {
  flake.modules.homeManager.vesktop = {pkgs, ...}: {
    programs.vesktop = {
      enable = true;
      package = pkgs.unstable.vesktop;

      settings = {
        autoUpdate = true;
        autoUpdateNotification = true;
        frameless = false;
        winCtrlQ = false;
        disableMinSize = false;
        winNativeTitleBar = false;
      };

      vencord = {
        # themes.Midnight = builtins.readFile ../../config/vesktop/vesktop-theme.css;

        settings = {
          transparent = true;
          useQuickCss = true;
          themeLinks = [];
          # enabledThemes = ["Midnight.css"];
          enableReactDevtools = false;
          plugins = {
            CommandsAPI.enabled = true;
          };
          notifications = {
            timeout = 5000;
            position = "bottom-right";
            useNative = "not-focused";
            logLimit = 50;
          };
          cloud = {
            authenticated = false;
            url = "https://api.vencord.dev/";
            settingsSync = false;
            settingsSyncVersion = 1718512206716;
          };
        };
      };
    };
  };

  flake.modules.nixos.vesktop = {
    home-manager.sharedModules = [
      self.modules.homeManager.vesktop
    ];
  };
}
