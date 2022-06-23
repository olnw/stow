autoload -Uz compinit promptinit
compinit
promptinit

PROMPT='%(?.%F{green}✓.%F{red}?%?)%f %B%F{240}%~%f%b %# '

alias em='emacs -nw'
alias emc='emacsclient -t'
alias autorm='sudo pacman -Rsn $(pacman -Qdtq)'
alias grep='grep --color=auto'
alias ll='ls -l --color=auto'
alias ls='ls -p --color=auto'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

