export EDITOR=emacsclient
export SUDO_EDITOR=$EDITOR
export VISUAL=$EDITOR
export BROWSER=firefox
export PATH=$PATH:$HOME/.local/bin:$HOME/.roswell/bin
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/keyring/ssh"
export MOZ_ENABLE_WAYLAND=1 # Enable Wayland for Firefox

# FCITX
export GLFW_IM_MODULE=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"
export SDL_IM_MODULE=ibus

