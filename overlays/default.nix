# This file defines overlays
{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs {pkgs = final;};

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
    awesome-git = prev.awesome.overrideAttrs (oldAttrs: {
      pname = "awesome-git";
      version = "cab3e81dc6071e3c1c4bd15cf8fab91236c7f2bd";
      src = prev.fetchFromGitHub {
        owner = "awesomeWM";
        repo = "awesome";
        rev = "cab3e81dc6071e3c1c4bd15cf8fab91236c7f2bd";
        sha256 = "sha256-DGAImB4u8sRP9PEoZ4YXAxopa8eaJ7YJxSiBh36yfaE=";
      };
      patches = [];
      postPatch = ''
        patchShebangs tests/examples/_postprocess.lua
      '';
    });

    # hyprland = inputs.hyprland.packages.${prev.system}.hyprland;
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
