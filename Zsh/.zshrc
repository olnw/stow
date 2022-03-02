autoload -Uz compinit promptinit
compinit
promptinit

prompt adam1

alias em='emacsclient'
alias info='info --vi-keys'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
