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

alias restart-audio 'pulseaudio -k && sudo alsactl restore'

#########################
######### Paths #########
#########################
set -Ux LUA_PATH '/usr/share/lua/5.4/?.lua;/usr/share/lua/5.4/?/init.lua;/usr/lib/lua/5.4/?.lua;/usr/lib/lua/5.4/?/init.lua;./?.lua;./?/init.lua;/home/ferreira/.luarocks/share/lua/5.4/?.lua;/home/ferreira/.luarocks/share/lua/5.4/?/init.lua'
set -Ux LUA_CPATH '/usr/lib/lua/5.4/?.so;/usr/lib/lua/5.4/loadall.so;./?.so;/home/ferreira/.luarocks/lib/lua/5.4/?.so'

set PATH $HOME/.local/bin $PATH
set PATH $HOME/.yarn/bin $PATH
set PATH $HOME/.npm-global/bin $PATH
set PATH $HOME/Android/Sdk/platform-tools $PATH

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set PATH "$GOPATH/bin" $PATH

set GEM_HOME (ruby -e 'puts Gem.user_dir')

set PATH "$GEM_HOME/bin" $PATH
set PATH "$HOME/.cargo/bin" $PATH
set PATH "$HOME/.luarocks/bin" $PATH
set PATH "$HOME/.deno/bin" $PATH

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

# pnpm
set -gx PNPM_HOME "/home/ferreira/.local/share/pnpm"
set -gx PATH "$PNPM_HOME" $PATH
# pnpm end

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true
