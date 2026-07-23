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
        # Pinned past nixpkgs' packaged "stable" (595.71.05), which has a
        # well-documented VRAM-exhaustion bug causing display freezes on
        # Wayland (audio/input keep working, only rendering hangs) -
        # triggered by repeated fullscreen toggling. 595.84 is NVIDIA's
        # current Recommended Driver and fixes several related game
        # hangs/black-screen regressions from the same 595 series.
        # mkDriver builds against our actual running kernel (via
        # config.boot.kernelPackages), same as nixpkgs' own "production"
        # entry - see https://discourse.nixos.org/t/screen-and-io-freezing-after-switching-from-24-11-to-25-05-on-nvidia-hardware/68257
        package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
          version = "595.84";
          sha256_64bit = "sha256-mcQE5SExvye8ptoCaNzOPr7cenOrF0BxqZXPGmxeugY=";
          sha256_aarch64 = "sha256-GloNdDFfmXFVu4FAlNNk2qzqLOuw2N5CKatKkcSrQxk=";
          openSha256 = "sha256-pEmA2tUcOKwUPKy6N0QvS49Pdut4/7Phs/JhjdyBcNY=";
          settingsSha256 = "sha256-QrnBM+sdWO4GanO62rxpHmRrjYkYpl5RD6fIiHq4C4A=";
          persistencedSha256 = "sha256-50xYdgx7EEThbaMp4QS8GADbxj0mhBXh8QQN0tWMwRg=";
        };
        powerManagement.enable = lib.mkForce false;
        powerManagement.finegrained = false;
      };
    };

    # hardware.nvidia-container-toolkit.enable = true;
  };
}
