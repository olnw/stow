autoload -Uz compinit promptinit
compinit
promptinit

# This will set the default prompt to the walters theme
prompt adam2

# alias startkeychain='eval $(keychain --eval --quiet id_ed25519 ~/.ssh/id_ed25519)'

# Edit and sync my NixOS config with my local Git repo
alias edit-conf='sudoedit /etc/nixos/configuration.nix && cp /etc/nixos/* ~/etc-nixos'

pfetch