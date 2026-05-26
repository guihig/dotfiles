{inputs, ...}: {
  flake-file.inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  flake.overlays = {
    unstable-packages = final: prev: {
      unstable = import inputs.nixpkgs-unstable {
        system = final.system;
        config.allowUnfree = true;
      };
    };

    modifications = final: prev: {
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
    };
  };
}
