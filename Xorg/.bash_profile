# Kill existing whoogle instances, then start whoogle.
kill $(pgrep whoogle-search)
/home/oliver/.local/bin/whoogle-search &

# Start fcitx
fcitx &

# For better font rendering
xsettingsd &

# Set desktop background
~/.fehbg &

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# Activate custom keymap table
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
