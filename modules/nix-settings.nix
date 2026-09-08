{
  self,
  inputs,
  lib,
  ...
}: {
  flake.modules.nixos.nix-settings = {
    nixpkgs = {
      overlays = [
        self.overlays.unstable-packages
        self.overlays.modifications
      ];

      config = {
        allowUnfree = true;
        permittedInsecurePackages = [];
        packageOverrides = pkgs: {
          vaapiIntel = pkgs.vaapiIntel.override {enableHybridCodec = true;};
        };
      };
    };

    nix.registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    nix.settings = {
      experimental-features = ["nix-command" "flakes"];
      auto-optimise-store = true;
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCUSeBc="
      ];
    };
  };
}
