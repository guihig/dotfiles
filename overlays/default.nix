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
      version = "aa8c7c6e27a20fa265d3f06c5dc3fe72cc5f021e";
      src = prev.fetchFromGitHub {
        owner = "awesomeWM";
        repo = "awesome";
        rev = "aa8c7c6e27a20fa265d3f06c5dc3fe72cc5f021e";
        sha256 = "sha256-DGAImB4u8sRP9PEoZ4YXAxopa8eaJ7YJxSiBh36yfaE=";
      };
      patches = [];
      postPatch = ''
        patchShebangs tests/examples/_postprocess.lua
      '';
    });

    hyprland = inputs.hyprland.packages.${prev.system}.hyprland;

    # vesktop = prev.vesktop.overrideAttrs (previousAttrs: {
    #   patches =
    #     previousAttrs.patches
    #     ++ [
    #       (prev.fetchpatch {
    #         name = "micfix-b0730e139805c4eea0d610be8fac28c1ed75aced.patch";
    #         url = "https://gist.githubusercontent.com/jvyden/4aa114a1118a06f3be96710df95f311c/raw/b0730e139805c4eea0d610be8fac28c1ed75aced/micfix.patch";
    #         hash = "sha256-EIK7/CtKpruf4/N2vn8XSCNkyDCL8I6ciXOljkvgz5A=";
    #       })
    #     ];
    # });
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
