# Kill existing whoogle instances, then start whoogle.
kill $(pgrep whoogle-search)
/home/oliver/.local/bin/whoogle-search &

# For better font rendering
xsettingsd &

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# Activate custom keymap table
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
