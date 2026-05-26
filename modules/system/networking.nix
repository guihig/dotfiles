{
  flake.modules.nixos.networking = {pkgs, ...}: {
    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = false;
        allowedTCPPorts = [3000];
      };
    };
  };
}
