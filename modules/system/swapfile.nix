{
  flake.modules.nixos.swapfile = {
    swapDevices = [
      {
        device = "/var/lib/swapfile";
        size = 8 * 1024;
      }
    ];

    boot.kernel.sysctl."vm.swappiness" = 10;
  };
}
