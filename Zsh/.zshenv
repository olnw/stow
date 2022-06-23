export BROWSER=firefox
export EDITOR=nvim
export SUDO_EDITOR=$EDITOR
export VISUAL=$EDITOR
export MOZ_ENABLE_WAYLAND=1 # Enable Wayland for Firefox
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/keyring/ssh"

# IME
export GLFW_IM_MODULE=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export SDL_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"

