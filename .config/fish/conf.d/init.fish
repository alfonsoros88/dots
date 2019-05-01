# bobthefish
set -g theme_nerd_fonts yes
set -g theme_powerline_fonts no
set -g theme_color_scheme gruvbox

# vi mode
fish_vi_key_bindings
bind -M insert \cf accept-autosuggestion
bind -M insert \cp history-search-backward

# GO
set -x -U GOPATH $HOME/go
set -xg PATH $PATH /usr/lib/go-1.11/bin

# Rust
source $HOME/.cargo/env

# dots
alias dots="/usr/bin/git --git-dir=$HOME/.dots.git/ --work-tree=$HOME"
