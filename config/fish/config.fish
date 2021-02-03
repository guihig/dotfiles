set -g fish_greeting

set BROWSER "firefox"

export EDITOR='vim'

# Get terminal emulator
# set TERM_EMULATOR (ps -aux | grep (ps -p $fish_pid -o ppid=) | awk 'NR==1{print $11}')

# Term
# switch "$TERM_EMULATOR"
# case '*kitty*'
#   export TERM='xterm-kitty'
# case '*alacritty*'
#     export TERM='xterm-256color'
# case '*'
#   export TERM='xterm-256color'
# end

#########################
####### Functions #######
#########################

#########################
######## Aliases ########
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

alias tmx tmuxinator

#########################
######### Paths #########
#########################
set PATH $HOME/.local/bin $PATH
set PATH $HOME/.yarn/bin $PATH
set PATH $HOME/.local/share/Steam/steamapps/common/Terraria $PATH

set JAVA_HOME /usr/lib/jvm/default

set PATH "$JAVA_HOME/bin" $PATH

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set PATH "$GOPATH/bin" $PATH

set PATH "$HOME/.npm-global/bin" $PATH
set GEM_HOME (ruby -e 'puts Gem.user_dir')
set PATH "$PATH:$GEM_HOME/bin"

set PATH "$PATH:$HOME/.cargo/bin"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
set PATH "$HOME/.rvm/bin" $PATH

#########################
######### Inits #########
#########################
if type -q starship
  starship init fish | source
end

eval (keychain --eval --quiet id_rsa ~/.ssh/id_rsa)

#cat ~/.cache/wal/sequences &
