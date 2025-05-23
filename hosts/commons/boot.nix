{
  config,
  pkgs,
  ...
}: {
  # Bootloader.
  boot.kernelParams = ["pcie_port_pm=off" "pcie_aspm.policy=performance"];
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = false;
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
      useOSProber = true;
      theme = pkgs.sleek-grub-theme.override {
        withStyle = "bigSur";
        withBanner = "GRUB MUITO BAITA";
      };
    };
  };
  boot.supportedFilesystems = ["ntfs"];
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
  ];
  boot.kernelModules = ["v4l2loopback"];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
  '';
}
