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
        extraLadspaPackages = [pkgs.deepfilternet];

        extraConfig.pipewire = {
          "99-deepfilter-noise-suppression" = {
            "context.modules" = [
              {
                name = "libpipewire-module-filter-chain";
                flags = ["nofail"];
                args = {
                  "node.description" = "DeepFilter Noise Canceling";
                  "media.name" = "DeepFilter Noise Canceling";
                  "filter.graph" = {
                    nodes = [
                      {
                        type = "ladspa";
                        name = "deepfilter";
                        plugin = "libdeep_filter_ladspa";
                        label = "deep_filter_stereo";
                        control = {
                          "Attenuation Limit (dB)" = 100.0;
                        };
                      }
                    ];
                  };
                  "audio.position" = ["FL" "FR"];
                  "capture.props" = {
                    "node.name" = "effect_input.deepfilter";
                    "node.passive" = true;
                  };
                  "playback.props" = {
                    "node.name" = "effect_output.deepfilter";
                    "media.class" = "Audio/Source";
                  };
                };
              }
            ];
          };
        };

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
  };
}
