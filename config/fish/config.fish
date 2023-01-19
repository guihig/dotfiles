set -g fish_greeting

#########################
####### Defaults ########
#########################
set BROWSER "firefox"
set EDITOR "vim"

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
######### Inits #########
#########################
source ~/.asdf/asdf.fish

if type -q starship
  starship init fish | source
end

if type -q keychain
  eval (keychain --eval --quiet id_rsa ~/.ssh/id_rsa)
end

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/ferreira/dev/misc/gcloud/google-cloud-sdk/path.fish.inc' ]; . '/home/ferreira/dev/misc/gcloud/google-cloud-sdk/path.fish.inc'; end

# Pywal
# cat ~/.cache/wal/sequences & (BORKED)

#########################
######### Paths #########
#########################
set -Ux LUA_PATH '/usr/share/lua/5.4/?.lua;/usr/share/lua/5.4/?/init.lua;/usr/lib/lua/5.4/?.lua;/usr/lib/lua/5.4/?/init.lua;./?.lua;./?/init.lua;/home/ferreira/.luarocks/share/lua/5.4/?.lua;/home/ferreira/.luarocks/share/lua/5.4/?/init.lua'
set -Ux LUA_CPATH '/usr/lib/lua/5.4/?.so;/usr/lib/lua/5.4/loadall.so;./?.so;/home/ferreira/.luarocks/lib/lua/5.4/?.so'
set GEM_HOME (ruby -e 'puts Gem.user_dir')
set PATH $HOME/.local/bin $PATH
set PATH $HOME/.yarn/bin $PATH
set PATH $HOME/.npm-global/bin $PATH
set PATH $HOME/Android/Sdk/platform-tools $PATH

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set PATH "$GOPATH/bin" $PATH

set PATH "$GEM_HOME/bin" $PATH
set PATH "$HOME/.cargo/bin" $PATH
set PATH "$HOME/.luarocks/bin" $PATH
set PATH "$HOME/.deno/bin" $PATH

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f $HOME/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin $HOME/.ghcup/bin $PATH

# pnpm
set -gx PNPM_HOME "$HOME/.local/share/pnpm"
set -gx PATH "$PNPM_HOME" $PATH
# pnpm end
