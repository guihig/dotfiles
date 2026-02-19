{
  # ---- Fish Configuration ---- #
  programs.fish = {
    enable = true;
    shellAliases = {
      pong = "ping 8.8.8.8";
      tmx = "tmuxinator";
      vim = "nvim";
      myip = "curl -fSsL 'https://api.ipify.org?format=json' | jq \".ip\"";
      cowalert = "xcowsay --monitor 1 comando: \" $(history | tail -n1 | grep -oP '\''(?<=  )[^;]++'\'' | head -n1) \" acabou ";
      toclip = "xclip -selection clipboard";
      granter = "cd /home/ferreira/dev/granter/";
      ls = "eza";
      ll = "eza -la";
      ip = "ip -c";
      ciasc-vpn = "sudo openfortivpn sslvpn01.ciasc.gov.br:443 --username=granter_gferreira@vpn.ciasc.gov.br";
    };
    functions = {
      ssh = {
        description = "SSH with xterm-256color";
        body = ''
          TERM=xterm-256color command ssh $argv
        '';
      };
    };
    interactiveShellInit = ''
      set fish_greeting
      set EDITOR "vim"

      nix-your-shell fish | source
    '';
  };
}
