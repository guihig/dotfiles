set BROWSER "firefox"
set -U EDITOR nvim
set fish_greeting

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
        --filters Name=tag:Type,Values=$argv[1]
end

#########################
######## Aliases ########f
#########################

function dc
    command docker-compose $argv
end

function dpf
    command docker ps --format "{{.Names}}"
end

function ssh
    TERM=xterm-256color command ssh $argv
end

function hossh
    TERM=xterm-256color command ssh -o ServerAliveInterval=60 -i $HOME/.ssh/horus-platform-kp.pem $argv
end

function toclip
    command xclip -selection c
end

function vim
    command nvim $argv
end

#########################
######### Paths #########
#########################
set PATH $PATH:$HOME/.local/bin
set PATH $PATH:$HOME/.yarn/bin
set PATH $PATH:$HOME/.local/share/Steam/steamapps/common/Terraria
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
set PATH "$PATH:$HOME/.rvm/bin"
set PATH "$PATH:/var/lib/snapd/snap/bin"

#########################
######### Inits #########
#########################
starship init fish | source

eval (keychain --eval --quiet id_rsa ~/.ssh/id_rsa)

cat ~/.cache/wal/sequences &
