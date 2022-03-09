autoload -Uz compinit promptinit
compinit
promptinit

prompt adam1

alias em='emacsclient'
alias info='info --vi-keys'
alias autoremove='sudo pacman -Rsn $(pacman -Qdtq)'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
