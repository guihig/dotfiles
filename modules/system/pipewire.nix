{
  flake.modules.nixos.pipewire = {pkgs, ...}: {
    security.rtkit.enable = true;
    services = {
      # Audio with pipewire
      pulseaudio.enable = false;
      pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;

        wireplumber.configPackages = [
          (pkgs.writeTextDir "share/wireplumber/main.lua.d/99-stop-microphone-auto-adjust.lua" ''
            table.insert (default_access.rules,{
                matches = {
                    {
                        { "application.process.binary", "=", "vesktop" },
                        { "application.process.binary", "=", "discord" }
                    }
                },
                default_permissions = "rx",
            })
          '')
        ];
      };
    };
    programs.noisetorch.enable = true;
  };
}
