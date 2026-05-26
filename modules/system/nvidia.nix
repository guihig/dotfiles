{
  flake.modules.nixos.nvidia = {
    lib,
    config,
    ...
  }: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };

    services.xserver.enable = true;
    services.xserver.videoDrivers = ["nvidia"];

    hardware = {
      nvidia = {
        modesetting.enable = true;
        open = false;
        nvidiaSettings = true;
        package = config.boot.kernelPackages.nvidiaPackages.stable;
        powerManagement.enable = lib.mkForce false;
        powerManagement.finegrained = false;
      };
    };

    # hardware.nvidia-container-toolkit.enable = true;
  };
}
