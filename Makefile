.PHONY: stow
stow:
	stow */

.PHONY: neovim
neovim:
	stow Neovim
	# Download vim-plug
	curl -fLo "$$HOME/.local/share/nvim/site/autoload/plug.vim" --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

	# Install vim-plug
	nvim --headless +PlugInstall +qa

.PHONY: system
system:
	sudo guix system reconfigure Guix/.config/guix/system.scm

.PHONY: home
home:
	guix home reconfigure Guix/.config/guix/home-configuration.scm

