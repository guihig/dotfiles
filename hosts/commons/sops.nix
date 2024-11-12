{
  # Sops Config
  sops = {
    age.sshKeyPaths = ["/etc/ssh/id_ed25519"];
    age.keyFile = "/home/ferreira/.config/sops/age/keys.txt";
    defaultSopsFile = ../../secrets/common/secrets.yaml;

    secrets."passwd/ferreira" = {
      neededForUsers = true;
    };
  };
}
