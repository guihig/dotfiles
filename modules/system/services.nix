{
  flake.modules.nixos.services = {pkgs, ...}: {
    services = {
      flatpak.enable = true;
      davfs2.enable = true;
      gvfs.enable = true;
      tumbler.enable = true;
      blueman.enable = true;
      spice-vdagentd.enable = true;

      # ollama = {
      #   enable = true;
      #   acceleration = "cuda";
      #   loadModels = ["deepseek-r1:8b" "qwq" "llama3"];
      # };

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

    # TSServer local
    # virtualisation.oci-containers = {
    #   backend = "docker";
    #   containers.teamspeak6-server = {
    #     image = "teamspeaksystems/teamspeak6-server:latest";
    #     autoStart = true;
    #     ports = [
    #       "9987:9987/udp" # voice
    #       "30033:30033/tcp" # file transfer
    #     ];
    #     environment = {
    #       TSSERVER_LICENSE_ACCEPTED = "accept";
    #       TSSERVER_DEFAULT_PORT = "9987";
    #       TSSERVER_VOICE_IP = "0.0.0.0";
    #       TSSERVER_FILE_TRANSFER_PORT = "30033";
    #       TSSERVER_FILE_TRANSFER_IP = "0.0.0.0";
    #     };
    #     volumes = ["teamspeak6-data:/var/tsserver"];
    #   };
    # };
    #
    # networking.firewall = {
    #   allowedUDPPorts = [9987];
    #   allowedTCPPorts = [30033];
    # };
  };
}
