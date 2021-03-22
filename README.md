# dotfiles
My Tiled WM config files

## Xmonad
The original xmonad config was built from this git repository:
* https://github.com/pjones/xmonadrc

I got some additional ideas from:
* https://github.com/altercation/dotfiles-tilingwm

## Neovim
To the neovim i got some ideas from 
* https://github.com/siduck76/neovim-dots -- (The galaxyline theme, so cool c:)
* https://github.com/s1n7ax/dotnvim -- (Mostly the Keybind, Variable, Options and Commands helper funcs)
* https://github.com/nanotee/nvim-lua-guide -- (A guide to help build neovim config in lua)

# Getting started

With python3 > on the $PATH

```
pip install dotbot --user
cd /path/to/dotfile
dotbot -c ./install.conf.yaml
```

# Requirements

#### Arch Based
```bash
sudo pacman -S picom alacritty fish rofi ranger tmux nodejs yarn stack go curl ninja elixir

# Ranger requirements
git clone https://github.com/alexanderjeurissen/ranger_devicons ~/.config/ranger/plugins/ranger_devicons
echo "default_linemode devicons" >> $HOME/.config/ranger/rc.conf
pip install pynvim --user

# AUR packages
paru -S polybar neovim-nightly-bin deadd-notification-center-bin nerd-fonts-jetbrains-mono

# Starship
curl -fsSL https://starship.rs/install.sh | bash

# GHCUP
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

# Neovim Config

### Lsp Installation

#### Treesitter
```bash
yarn global add tree-sitter-cli
```

#### Json Lsp
```bash
yarn global add vscode-json-languageserver
```

#### CSS Lsp
```bash
yarn global add vscode-css-languageserver-bin
```

#### Typescript Lsp
```bash
yarn global add typescript-language-server
```

#### Vue Lsp
```bash
yarn global add vls
```

#### Haskell
```bash
ghcup install hls
```

#### Elixir
```bash
curl -fLO https://github.com/elixir-lsp/elixir-ls/releases/latest/download/elixir-ls.zip
unzip elixir-ls.zip -d ~/.lsp/elixir-ls
chmod +x ~/.lsp/elixir-ls/language_server.sh
```

#### Lua Lsp
```bash
cd ~/.lsp
git clone https://github.com/sumneko/lua-language-server
cd lua-language-server
git submodule update --init --recursive

cd 3rd/luamake
ninja -f ninja/linux.ninja
cd ../..
./3rd/luamake/luamake rebuild
```

#### Efm Lsp
```bash
go get github.com/mattn/efm-langserver
```
