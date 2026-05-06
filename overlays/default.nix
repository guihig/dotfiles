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
    awesome = prev.awesome.overrideAttrs (oldAttrs: {
      version = "39143f036e10c34c0a4a2eb604399fd20cf8376e";
      src = prev.fetchFromGitHub {
        owner = "awesomeWM";
        repo = "awesome";
        rev = "39143f036e10c34c0a4a2eb604399fd20cf8376e";
        sha256 = "sha256-M9EQV5kwiTeSqZLXQ6uAtMyvOhCQXM9ctgiu2ZRI+QQ=";
      };
      cmakeFlags = (oldAttrs.cmakeFlags or []) ++ ["-DCMAKE_POLICY_VERSION_MINIMUM=3.5"];
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
