{
  flake.modules.nixos.fortclient = {
    environment.etc."ppp/options".text = "ipcp-accept-remote";
  };
}
