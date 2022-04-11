;; Eventually I'd like to switch entirely from GNU Stow to Guix Home,
;; but this will be a gradual process. Guix Home is also very new,
;; and doesn't have many built-in service types.
(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells))

(home-environment
  (packages
    (map (compose list specification->package+output)
         (list 
           ;; Themes
           "adwaita-icon-theme"
           "gnome-themes-standard"
           "hicolor-icon-theme"

           ;; 3D & CAD
           "blender"

           ;; Audio
           "ardour"
           "pipewire"
           "pulseaudio"
           "wireplumber"

           ;; Image Editing & Digital Art
           "gimp"
           "krita"

           ;; Video
           "mpv"
           "mpv-mpris" ; With this plugin, playerctl can be used with mpv.
           "playerctl"
           "gstreamer"
           "gst-plugins-base"
           "gst-plugins-good"
           ;; Might need both or either of these if videos won't play.
           ;;"gst-plugins-bad"
           ;;"gst-plugins-ugly"
           "gst-libav"

           ;; Browsers
           "icecat"
           "ungoogled-chromium"

           ;; C/C++
           "clang"
           "cmake"
           "emacs-ccls"
           "gcc-toolchain"
           "glibc-locales"
           "lld"
           "llvm"
           "make"
           "meson"
           "ncurses"
           "ninja"
           "pkg-config"

           ;; Python
           "python2"
           "python"
           "python-lsp-server"

           ;; CLI Tools
           "curl"
           "duplicity"
           "ffmpeg"
           "file"
           "flatpak"
           "git"
           "gnome-keyring"
           "htop"
           "openssh"
           "p7zip"
           "pfetch"
           "stow"
           "syncthing"
           "the-silver-searcher"
           "tree"
           "wev"
           "yt-dlp"

           ;; Text Editors
           "emacs-next-pgtk"
           "emacs-vterm"
           "neovim"

           ;; Fonts
           "font-adobe-source-han-sans"
           "font-fira-go"
           "font-google-noto"
           "font-iosevka"
           "font-iosevka-aile"
           "font-jetbrains-mono"

           ;; Gaming
           ;;"steam-nvidia"

           ;; Misc. GUI Applications
           "keepassxc"
           "piper")))

  (services
    (list (service home-bash-service-type
	    (home-bash-configuration
              (environment-variables '(("KITTY_ENABLE_WAYLAND"    . "1")
                                       ("WLR_NO_HARDWARE_CURSORS" . "1") ; Fix disappearing cursor in Sway
                                       ("SSH_AUTH_SOCK"           . "${XDG_RUNTIME_DIR}/keyring/ssh")
                                       ("EDITOR"                  . "emacsclient")
                                       ("SUDO_EDITOR"             . "$EDITOR")
                                       ("VISUAL"                  . "$EDITOR")
                                       ("BROWSER"                 . "chromium")))
                                       ;; IME
                                       ;;("GLFW_IM_MODULE" . "fcitx5")
                                       ;;("GTK_IM_MODULE"  . "fcitx5")
                                       ;;("SDL_IM_MODULE"  . "fcitx5")
                                       ;;("QT_IM_MODULE"   . "fcitx5")
                                       ;;("XMODIFIERS"     . "@im=fcitx5")
      
              (aliases '(("emc" . "emacsclient -t")
                         ("em"  . "emacs -nw"))))))))
      
