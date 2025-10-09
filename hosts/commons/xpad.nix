{pkgs, ...}: {
  boot = {
    kernelModules = ["xpad"];
  };

  services.udev.extraRules = ''
    # 2.4GHz/Dongle
    KERNEL=="hidraw*", ATTRS{idVendor}=="2dc8", MODE="0660", TAG+="uaccess"
    # Bluetooth
    KERNEL=="hidraw*", KERNELS=="*2DC8:*", MODE="0660", TAG+="uaccess"
  '';
}
