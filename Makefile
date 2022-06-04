.PHONY: stow
stow:
	stow */

.PHONY: system
system:
	sudo guix system reconfigure Guix/.config/guix/system.scm

.PHONY: home
home:
	guix home reconfigure Guix/.config/guix/home-configuration.scm

