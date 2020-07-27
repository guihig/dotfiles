set BROWSER "firefox"
set -U EDITOR nvim

#########################
####### Functions #######
#########################

function awsid
    echo '051630741961'
end

function mappa-server-ip-by-tag
    command aws ec2 describe-instances \
        --query='Reservations[0].Instances[].NetworkInterfaces[].PrivateIpAddresses[].Association.PublicIp' \
        --output=text \
        --filters Name=tag:Type,Values=$1
end

#########################
######## Aliases ########
#########################
alias cowalert 'xcowsay comando: " (history | tail -n1 | grep -oP '\''(?<=  )[^;]++'\'' | head -n1) " acabou '
alias dc 'docker-compose '
alias dpf 'docker ps --format "{{.Names}}"'
alias ssh 'TERM=xterm-256color ssh'
alias hossh 'ssh -o ServerAliveInterval=60 -i $HOME/.ssh/horus-platform-kp.pem '
alias toclip 'xclip -selection c'
alias vim 'nvim'

#########################
######### Paths #########
#########################
set PATH $PATH:/home/ferreira/.local/bin
set PATH $PATH:/home/ferreira/.yarn/bin
set PATH $PATH:/home/ferreira/.local/share/Steam/steamapps/common/Terraria
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
set PATH "$PATH:$HOME/.rvm/bin"

#########################
######### Inits #########
#########################
starship init fish | source

eval (keychain --eval --quiet id_rsa ~/.ssh/id_rsa)
