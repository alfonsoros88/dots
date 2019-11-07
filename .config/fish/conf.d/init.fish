set -xg EDITOR vi

# bobthefish
set -g theme_nerd_fonts yes
set -g theme_powerline_fonts no
set -g theme_color_scheme gruvbox

# GO
set -x -U GOPATH $HOME/go
set -xg PATH $PATH $GOPATH/bin

# Rust
source $HOME/.cargo/env

# cabal
set -xg PATH $PATH $HOME/.cabal/bin

# nodejs
set -xg  NVM_DIR $HOME/.nvm

# dots
alias dots="/usr/bin/git --git-dir=$HOME/.dots.git/ --work-tree=$HOME"
alias dotss="dots status -s"

alias cat="bat"
alias ls="exa"

set -xg MANPAGER "sh -c 'col -bx | bat -l man -p'"

# bazel alias
function bz --wraps bazel
    set werkstatt (git rev-parse --show-toplevel 2>/dev/null)
    $werkstatt/tools/docker/bazel.py $argv
end

function bd --wraps bazel
    set werkstatt (git rev-parse --show-toplevel 2>/dev/null)
    $werkstatt/tools/docker/bazel_gdbserver.sh -c dbg $argv
end

abbr -a -g bb bz build
abbr -a -g bt bz test --cache_test_results=no
abbr -a -g br bz run

#alias k='`git rev-parse --show-toplevel 2>/dev/null`/tools/docker/killrunning.py'
#alias dr='`git rev-parse --show-toplevel 2>/dev/null`/tools/docker/run_in_docker.py'

function __bazel_tests
    set target (bz query 'kind("cc_.*", //...)' | fzf)
    echo $target
end
complete -f -c bt -a '(__bazel_tests ".*")'

set -gx FZF_DEFAULT_COMMAND 'rg --files --ignore-vcs -g "!bazel-*"'
set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -gx FZF_ALT_C_COMMAND 'rg --sort-files --files --ignore-vcs -g "!bazel-*" --null 2> /dev/null | xargs -0 dirname | uniq'

alias cdb="tools/docker/run_in_docker.sh tools/style/generate_compile_commands.sh"

set -xg RTI_LICENSE_FILE $HOME/rti_license/rti_license.dat
