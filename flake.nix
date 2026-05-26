# DO-NOT-EDIT. This file was auto-generated using github:vic/flake-file.
# Use `nix run .#write-flake` to regenerate it.
{
  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);

  inputs = {
    awesome-wm-widgets = {
      url = "github:streetturtle/awesome-wm-widgets";
      flake = false;
    };
    bling = {
      url = "github:BlingCorp/bling";
      flake = false;
    };
    color = {
      url = "github:andOrlando/color";
      flake = false;
    };
    expert.url = "github:elixir-lang/expert";
    flake-file.url = "github:vic/flake-file";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    import-tree.url = "github:vic/import-tree";
    lain = {
      url = "github:lcpz/lain";
      flake = false;
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    rubato = {
      url = "github:andOrlando/rubato";
      flake = false;
    };
    sops-nix.url = "github:mic92/sops-nix";
  };
}
