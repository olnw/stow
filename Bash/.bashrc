pfetch
export EDITOR=emacs
# I'm using GNOME keyring at the moment
# alias startkeychain='eval $(keychain --eval --quiet id_ed25519 ~/.ssh/id_ed25519)'
# Sync my NixOS config to my Git repo directory
alias sync-conf='cp /etc/nixos/* ~/etc-nixos'
alias edit-conf='sudoedit /etc/nixos/configuration.nix'
