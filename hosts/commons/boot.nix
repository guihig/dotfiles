{pkgs, ...}: {
  # Bootloader.
  boot.kernelParams = ["pcie_port_pm=off" "pcie_aspm.policy=performance"];
  boot.loader = {
    efi.canTouchEfiVariables = true;
    # systemd-boot.enable = true;
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
}
