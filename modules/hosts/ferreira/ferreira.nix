{
  inputs,
  self,
  ...
}: {
  flake.nixosConfigurations.ferreira-pc = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.ferreira-hardware
      self.modules.nixos.ferreira
    ];
  };

  flake.modules.nixos.ferreira-system = {
    imports = [
      self.modules.nixos.nix-settings
      self.modules.nixos.boot
      self.modules.nixos.locale
      self.modules.nixos.nvidia
      self.modules.nixos.core
      self.modules.nixos.networking
      self.modules.nixos.swapfile
      self.modules.nixos.services
      self.modules.nixos.pipewire
    ];

    hardware = {
      i2c.enable = true;
      bluetooth = {
        enable = true;
      };
      keyboard.qmk.enable = true;
    };
  };

  flake.modules.nixos.ferreira-programs = {pkgs, ...}: {
    imports = [
      self.modules.nixos.ferreira-packages

      self.modules.nixos.fortclient
      self.modules.nixos.git
      self.modules.nixos.ssh

      self.modules.nixos.vesktop
      self.modules.nixos.ranger
      self.modules.nixos.nvim
      self.modules.nixos.fish
      self.modules.nixos.kitty
      self.modules.nixos.tmux

      self.modules.nixos.niri
      # self.modules.nixos.awesomewm
      # self.modules.nixos.hyprland
    ];
  };

  flake.modules.nixos.ferreira = {
    lib,
    pkgs,
    ...
  }: {
    imports = [
      inputs.home-manager.nixosModules.default

      self.modules.nixos.ferreira-system
      self.modules.nixos.ferreira-programs
    ];

    networking.hostName = "ferreira-pc";

    users.users.ferreira = {
      isNormalUser = true;
      initialPassword = "1234";
      description = "CABECA";
      extraGroups = ["networkmanager" "wheel" "docker" "libvirtd" "i2c"];
      shell = pkgs.fish;
    };

    system.stateVersion = "25.11";

    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.backupFileExtension = "backup";
    home-manager.backupCommand = "rm";
    home-manager.overwriteBackup = true;

    home-manager.users.ferreira = {
      home = {
        username = "ferreira";
        homeDirectory = "/home/ferreira";
        sessionPath = ["$HOME/.local/bin"];
        sessionVariables = {
          TERMINAL = "kitty";
          EDITOR = "nvim";
          BROWSER = "firefox";
        };

        stateVersion = "25.11";
      };

      systemd.user.startServices = "sd-switch";
    };
  };
}
