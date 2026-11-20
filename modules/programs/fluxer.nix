{self, ...}: {
  flake.modules.homeManager.fluxer = {
    pkgs,
    lib,
    ...
  }: let
    pname = "fluxer";
    version = "2026.1120.30521";

    src = pkgs.fetchurl {
      url = "https://pkgs.fluxer.com/desktop/stable/linux/x64/${version}/appimage";
      hash = "sha256-z5jaOxANMWEMY37sjVW6mEdOQNNdRfOctZ2252ia66o=";
    };

    appimageContents = pkgs.appimageTools.extractType2 {inherit pname version src;};
  in {
    home.packages = [
      (pkgs.appimageTools.wrapType2 {
        inherit pname version src;

        extraPkgs = pkgs: with pkgs; [libnotify at-spi2-atk libayatana-appindicator];

        extraInstallCommands = ''
          install -Dm444 ${appimageContents}/app.fluxer.FluxerDesktop.desktop $out/share/applications/fluxer.desktop
          install -Dm444 ${appimageContents}/fluxer.png $out/share/icons/hicolor/512x512/apps/fluxer.png
          substituteInPlace $out/share/applications/fluxer.desktop \
            --replace-fail 'Exec=AppRun %U' 'Exec=fluxer %U' \
            --replace-fail '/opt/Fluxer/fluxer' 'fluxer'
        '';
      })
    ];
  };

  flake.modules.nixos.fluxer = {
    home-manager.sharedModules = [
      self.modules.homeManager.fluxer
    ];
  };
}
