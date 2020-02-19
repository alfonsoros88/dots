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

# virtualfish
eval (python3 -m virtualfish)

# dots
alias dots="/usr/bin/git --git-dir=$HOME/.dots.git/ --work-tree=$HOME"
alias dotss="dots status -s"
alias dotsu="dots add -u && dots commit -m \"update\""

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

function dr --wraps fish
    set werkstatt (git rev-parse --show-toplevel 2>/dev/null)
    $werkstatt/tools/docker/run_in_docker.py $argv
end

function bzd
    set werkstatt (git rev-parse --show-toplevel 2>/dev/null)
    pushd $werkstatt
    bz run -c dbg --script_path=.dbg.sh --run_under='gdbserver :2008' $argv && dr (pwd)/.dbg.sh
    popd
end

abbr -a -g bb bz build
abbr -a -g bt bz test --cache_test_results=no
abbr -a -g br bz run

function fco --description "checkout branch"
  git branch --all | grep -v HEAD | string trim | fzf | read -l result; and git checkout "$result"
end

#alias k='`git rev-parse --show-toplevel 2>/dev/null`/tools/docker/killrunning.py'

function __bazel_tests
    set target (bz query 'kind("cc_.*", //...)' | fzf)
    echo $target
end
complete -f -c bt -a '(__bazel_tests ".*")'

set -gx FZF_DEFAULT_OPTS "--ansi --preview-window 'right:60%'"
set -gx FZF_DEFAULT_COMMAND 'rg -i --files --ignore-vcs -g "!bazel-*"'
set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -gx FZF_CTRL_T_OPTS "--preview 'bat --color=always --style=header,grid --line-range :300 {}'"
set -gx FZF_ALT_C_COMMAND 'rg --sort-files --files --ignore-vcs -g "!bazel-*" --null 2> /dev/null | xargs -0 dirname | uniq'

function cdb --description "generate compile database and bazel targets"
    set werkstatt (git rev-parse --show-toplevel 2>/dev/null)
    $werkstatt/tools/docker/run_in_docker.sh tools/style/generate_compile_commands.sh $argv
    # bz query --noshow_progress 'kind("cc_.*", //...)' 2>/dev/null > .cache/bazel_targets.log
end

set -xg RTI_LICENSE_FILE $HOME/rti_license/rti_license.dat

function lgdb --description "debug target locally"
    bz run --script_path=foo.sh -c dbg $argv[1]

    set -l runfiles_dir (rg 'RUNFILES_DIR=([^\s]+)' foo.sh -r '$1' -o -N)
    set -l runfile (string replace ':' '/' (string replace -r '^//?' '' $argv[1]))

    pushd $runfiles_dir/werkstatt
    gdb -x 'directory ~/werkstatt' --args $runfile $argv[2..-1]
    popd
    rm foo.sh
end
