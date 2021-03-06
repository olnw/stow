(use-modules (gnu)
             (gnu packages linux)
             (gnu packages disk)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (ice-9 rdelim)
             (guix build-system meson)
             (guix build-system cargo))

(use-service-modules desktop networking ssh xorg dbus sddm sound)
(use-package-modules gnome)

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
  (host-name "rms-our-saviour")
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
      (map specification->package
           (list
             "nss-certs"
             "openbox"
             "obconf"
             "xterm"
             "sway"
             "swaybg"
             "swayidle"
             "swaylock-effects"
             "wofi"
             ;;"tlp"
             "gnome-keyring"
             "git"
             "stow"
             "emacs"
             "dconf" ; Fixes warnings when running seahorse
             "ntfs-3g"
             "dosfstools" ; provides mkfs.fat
             "libratbag")) ; needed for piper
      %base-packages))

  (services
    (append
      (list (service gnome-keyring-service-type)

            ;; dconf and libratbag are needed for piper
            (simple-service 'my-dbus-root-services dbus-root-service-type (list dconf libratbag))

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
              (uuid "5d34455f-d9da-435b-80bf-6c2eef136e41"))
            (target "cryptroot")
            (type luks-device-mapping))))

  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "C440-05BA" 'fat32))
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

