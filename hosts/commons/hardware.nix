{...}: {
  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    bluetooth = {
      enable = true;
    };
    pulseaudio.enable = false;
    keyboard.qmk.enable = true;
  };

  # Swapfile
  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 8 * 1024;
    }
  ];
}
