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
        cudaSupport = true;
        permittedInsecurePackages = ["ventoy-1.1.10" "mbedtls-2.28.10"];
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
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://cache.garnix.io"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCUSeBc="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
      ];
    };
  };
}
