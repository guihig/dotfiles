{
  self,
  inputs,
  ...
}: {
  flake-file.inputs = {
    sops-nix.url = "github:mic92/sops-nix";
  };

  flake.modules.homeManager.ssh = {config, ...}: {
    imports = [
      inputs.sops-nix.homeManagerModule
    ];

    home.file.".ssh/config" = {
      source = ./configs/config;
    };

    sops = {
      age.sshKeyPaths = ["/etc/ssh/id_ed25519"];
      age.keyFile = "/home/ferreira/.config/sops/age/keys.txt";
      defaultSopsFile = ../../../../secrets/common/secrets.yaml;

      secrets."ssh_keys/id_rsa/priv" = {
        path = "${config.home.homeDirectory}/.ssh/id_rsa";
        mode = "0600";
      };
      secrets."ssh_keys/id_rsa/pub" = {
        path = "${config.home.homeDirectory}/.ssh/id_rsa.pub";
        mode = "0644";
      };

      secrets."ssh_keys/ciasc_rsa/priv" = {
        path = "${config.home.homeDirectory}/.ssh/ciasc_rsa";
        mode = "0600";
      };
      secrets."ssh_keys/ciasc_rsa/pub" = {
        path = "${config.home.homeDirectory}/.ssh/ciasc_rsa.pub";
        mode = "0644";
      };

      secrets."ssh_keys/owls_cloud/pem" = {
        path = "${config.home.homeDirectory}/.ssh/owls_cloud.pem";
        mode = "0600";
      };

      secrets."ssh_keys/google_compute_engine/priv" = {
        path = "${config.home.homeDirectory}/.ssh/google_compute_engine";
        mode = "0600";
      };
      secrets."ssh_keys/google_compute_engine/pub" = {
        path = "${config.home.homeDirectory}/.ssh/google_compute_engine.pub";
        mode = "0644";
      };
    };
  };

  flake.modules.nixos.ssh = {
    home-manager.sharedModules = [
      self.modules.homeManager.ssh
    ];

    services.openssh = {
      enable = true;
      settings.PermitRootLogin = "no";
    };
  };
}
