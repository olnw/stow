export EDITOR=emacsclient
export SUDO_EDITOR=$EDITOR
export VISUAL=$EDITOR
export BROWSER=firefox
export PATH=$HOME/.local/bin:$PATH
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/keyring/ssh"

# FCITX
export GLFW_IM_MODULE=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"
export SDL_IM_MODULE=ibus

