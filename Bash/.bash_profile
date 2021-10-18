# pam_env no longer reads ~/.pam_environment (deprecated)
# so I will use .bash_profile for environment variables.

export VISUAL=emacs
export EDITOR="$VISUAL"
export BROWSER=firefox

# FCITX
export GTK_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5
export XMODIFIERS="@im=fcitx5"

# For better font rendering
xsettingsd &

# Activate custom keymap table
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
