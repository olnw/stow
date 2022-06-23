autoload -Uz compinit promptinit
compinit
promptinit

PROMPT='%(?.%F{green}✓.%F{red}?%?)%f %B%F{240}%~%f%b %# '

alias autorm='sudo pacman -Rsn $(pacman -Qdtq)'
alias em='emacs -nw'
alias emc='emacsclient -t'
alias grep='grep --color=auto'
alias ll='ls -l --color=auto'
alias ls='ls -p --color=auto'
alias vlime_sbcl='rlwrap sbcl --load ${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/packer/start/vlime/lisp/start-vlime.lisp'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

