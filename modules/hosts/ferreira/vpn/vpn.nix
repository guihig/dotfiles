{
  self,
  inputs,
  ...
}: {
  flake.modules.homeManager.vpn = {config, ...}: {
    imports = [
      inputs.sops-nix.homeManagerModule
    ];

    sops = {
      age.sshKeyPaths = ["/etc/ssh/id_ed25519"];
      age.keyFile = "/home/ferreira/.config/sops/age/keys.txt";
      defaultSopsFile = ../../../../secrets/common/secrets.yaml;

      secrets."ovpn/ciasc/config" = {
        path = "${config.home.homeDirectory}/dev/vpns/granter_gferreira@vpn.ciasc.gov.br.ovpn";
        mode = "0600";
      };
    };
  };

  flake.modules.nixos.vpn = {
    home-manager.sharedModules = [
      self.modules.homeManager.vpn
    ];
  };
}
