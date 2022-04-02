autoload -Uz compinit promptinit
compinit
promptinit

PROMPT='%(?.%F{green}✓.%F{red}?%?)%f %B%F{240}%~%f%b %# '

alias em='emacsclient -t'
alias info='info --vi-keys'
alias autoremove='sudo pacman -Rsn $(pacman -Qdtq)'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
