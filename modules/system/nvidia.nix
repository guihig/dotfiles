{
  flake.modules.nixos.nvidia = {
    lib,
    config,
    pkgs,
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
        # Pinned past nixpkgs' packaged "stable" (595.71.05), which has a
        # well-documented VRAM-exhaustion bug causing display freezes on
        # Wayland (audio/input keep working, only rendering hangs) -
        # triggered by repeated fullscreen toggling. 610.43.03 is nixpkgs
        # unstable's new_feature/beta branch, newer than the 595 series.
        # Built via nixpkgs-unstable's nvidia-x11 recipe, but bound to our
        # actual running kernel (config.boot.kernelPackages.kernel, from the
        # stable input) rather than unstable's own kernel, so the module ABI
        # matches what's actually booted - see
        # https://discourse.nixos.org/t/screen-and-io-freezing-after-switching-from-24-11-to-25-05-on-nvidia-hardware/68257
        package =
          (pkgs.unstable.linuxPackagesFor config.boot.kernelPackages.kernel).nvidiaPackages.new_feature;
        powerManagement.enable = lib.mkForce false;
        powerManagement.finegrained = false;
      };
    };

    # hardware.nvidia-container-toolkit.enable = true;
  };
}
