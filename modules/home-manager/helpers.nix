{
  inputs,
  lib,
  config,
  ...
}:
with lib; {
  # https://github.com/ncfavier/config/blob/954cbf4f569abe13eab456301a00560d82bd0165/modules/nix.nix#L12-L14
  config = {
    lib.meta = {
      configPath = "${config.home.homeDirectory}/dotfiles";
      mkMutableSymlink = path:
        config.lib.file.mkOutOfStoreSymlink
        (config.lib.meta.configPath + removePrefix (toString inputs.self) (toString path));
    };
  };
}
