(use-modules (gnu)
	     (nongnu packages linux)
	     (nongnu system linux-initrd)
             (ice-9 rdelim)
	     (guix build-system meson)
	     (guix build-system cargo))

(use-service-modules desktop networking ssh xorg dbus sddm sound)
(use-package-modules wm terminals version-control emacs package-management vim gnome xdisorg)

;; https://lists.gnu.org/archive/html/help-guix/2021-04/msg00040.html
(define steven-black-hosts (with-input-from-file "/home/oliver/.config/guix/hosts"
                                                 (lambda () (read-delimited ""))))

(operating-system
  (kernel linux)
  (initrd (lambda (file-systems . rest)
	    (apply microcode-initrd file-systems
		   #:initrd base-initrd
		   #:microcode-packages (list amd-microcode)
		   rest)))
  (firmware (list linux-firmware))

  (locale "en_AU.utf8")
  (timezone "Australia/Hobart")
  (keyboard-layout (keyboard-layout "au"))
  (host-name "guix-pc")
  (users (cons* (user-account
                  (name "oliver")
                  (comment "Oliver")
                  (group "users")
                  (home-directory "/home/oliver")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Core packages. Available to all users.
  (packages
    (append
      (list (specification->package "nss-certs")
            sway
            swaybg
            swayidle
            swaylock-effects
	    wofi
	    foot
            ;;tlp
            gnome-keyring
            kitty
            git
            stow
            neovim
            emacs
            dconf) ; Fixes warnings when running seahorse
      %base-packages))

  (services
    (append
      (list (service gnome-keyring-service-type)

            ;; dconf and libratbag are needed for piper
            (simple-service 'my-dbus-services dbus-root-service-type (list dconf libratbag))

            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))

            (modify-services %desktop-services
                            (gdm-service-type config =>
                                              (gdm-configuration
                                                (inherit config)
                                                (wayland? #t)))

                            (pulseaudio-service-type config =>
                                                     (pulseaudio-configuration
                                                       (inherit config)
                                                       (client-conf '((autospawn . no)))))

                            (guix-service-type config =>
                                               (guix-configuration
                                                 (inherit config)
                                                 (substitute-urls
                                                   (append (list "https://substitutes.nonguix.org")
                                                           %default-substitute-urls))
                                                 (authorized-keys
                                                   (append (list (local-file "./signing-key.pub"))
                                                           %default-authorized-guix-keys)))))))

  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))

  (mapped-devices
    (list (mapped-device
            (source
              (uuid "da689bfb-a1d4-4869-9ea9-c09342dccb4d"))
            (target "cryptroot")
            (type luks-device-mapping))))

  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "49E6-3E3E" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
	   (file-system
	     (mount-point "/mnt/hdd")
	     (device "/dev/sda1")
	     (type "ext4"))
           %base-file-systems))

  (hosts-file (plain-file "hosts"
                          (string-append (local-host-aliases host-name)
                                         steven-black-hosts)))

  ;; Show asterisks while entering a password
  (sudoers-file (plain-file "sudoers"
                            (string-append (plain-file-content %sudoers-specification)
                                           "Defaults pwfeedback\n"))))

