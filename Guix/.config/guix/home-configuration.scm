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
	   "piper"))))

