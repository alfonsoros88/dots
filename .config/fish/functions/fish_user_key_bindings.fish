function fish_user_key_bindings
    fzf_key_bindings
    fish_vi_key_bindings
    bind -M insert \cf accept-autosuggestion
    bind -M insert \cp history-search-backward
end
