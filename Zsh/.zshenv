export EDITOR=emacsclient
export SUDO_EDITOR=$EDITOR
export VISUAL=$EDITOR
export BROWSER=chromium
export PATH=/home/oliver/.local/bin:$PATH

export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"

# FCITX
export GTK_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5
export XMODIFIERS="@im=fcitx5"
export SDL_IM_MODULE=fcitx5
