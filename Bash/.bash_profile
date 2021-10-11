# For better font rendering
xsettingsd &

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# Activate custom keymap table
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
