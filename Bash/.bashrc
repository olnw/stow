#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '


alias autoremove='sudo pacman -Rsn $(pacman -Qdtq)'
eval "$(thefuck --alias)"

alias pc='sudo pacman'

export TERM=xterm-256color

# For use with commands such as: sudoedit <filename>
# (allows you to use your config).
export EDITOR=nvim

alias sk='keychain --eval --quiet id_ed25519 ~/.ssh/id_ed25519'
