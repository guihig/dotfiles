{
  flake.modules.nixos.networking = {pkgs, ...}: {
    networking = {
      networkmanager = {
        enable = true;
        plugins = [pkgs.networkmanager-openvpn];
      };
      firewall = {
        enable = false;
        allowedTCPPorts = [3000];
      };
    };
  };
}
