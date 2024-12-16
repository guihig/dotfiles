{
  lib,
  config,
  ...
}: {
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
}
