set -g fish_greeting
set BROWSER "firefox"

export EDITOR='vim'

# Get terminal emulator
set TERM_EMULATOR (ps -aux | grep (ps -p $fish_pid -o ppid=) | awk 'NR==1{print $11}')

# Term
switch "$TERM_EMULATOR"
case '*kitty*'
	export TERM='xterm-kitty'
case '*alacritty*'
    export TERM='alacritty'
case '*'
	export TERM='xterm-256color'
end

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

alias vim nvim

alias peixe fish
#########################
######### Paths #########
#########################
set PATH $HOME/.local/bin $PATH
set PATH $HOME/.yarn/bin $PATH
set PATH $HOME/.local/share/Steam/steamapps/common/Terraria $PATH
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
set PATH "$HOME/.rvm/bin" $PATH
set PATH "/var/lib/snapd/snap/bin" $PATH

set JAVA_HOME /usr/lib/jvm/default
set ANDROID_HOME "$HOME/Android/Sdk"

set PATH "$JAVA_HOME/bin" $PATH

set PATH "$ANDROID_HOME/tools" $PATH
set PATH "$ANDROID_HOME/platform-tools" $PATH
set PATH "$ANDROID_HOME/tools/bin" $PATH
set PATH "$ANDROID_HOME/emulator" $PATH

set ANDROID_SDK_ROOT "$ANDROID_HOME"

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set PATH "$GOPATH/bin" $PATH
# set PATH "$HOME/squashfs-root/usr/bin" $PATH

#########################
######### Inits #########
#########################
starship init fish | source

eval (keychain --eval --quiet id_rsa ~/.ssh/id_rsa)

#cat ~/.cache/wal/sequences &
