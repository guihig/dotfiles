set -g fish_greeting

set BROWSER "firefox"

export EDITOR='vim'

#########################
####### Functions #######
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

function toclip
    command xclip -selection c
end

#########################
######## Aliases ########
#########################
alias vim nvim

alias tmx tmuxinator

alias pong "ping 8.8.8.8"

#########################
######### Paths #########
#########################
set PATH $HOME/.local/bin $PATH
set PATH $HOME/.yarn/bin $PATH
set PATH $HOME/local/nvim/bin $PATH
set PATH $HOME/local/elixir/bin $PATH

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set PATH "$GOPATH/bin" $PATH

set GEM_HOME (ruby -e 'puts Gem.user_dir')

set PATH "$PATH:$GEM_HOME/bin"
set PATH "$PATH:$HOME/.cargo/bin"
set PATH "$HOME/.luarocks/bin" $PATH

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
set PATH "$HOME/.rvm/bin" $PATH

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/ferreira/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/ferreira/.ghcup/bin $PATH
#########################
######### Inits #########
#########################

source ~/.asdf/asdf.fish

if type -q starship
  starship init fish | source
end

if type -q keychain
  eval (keychain --eval --quiet id_rsa ~/.ssh/id_rsa)
end

#cat ~/.cache/wal/sequences &
