# Install a program via apt-get

debug = $(if $(MAKE_DEBUG),--debug)
installer = @perl install/install.pl $(debug) $(1)

install_prog = $(call installer,install) $(1)
linkf = $(call installer,link) $(1) $(2)
link_contents = $(call installer,link_contents) $(1) $(2)

# Full Configuration
.PHONY: configure_all
configure_all : configure_dev_all configure_user_all

# Full development configuration
.PHONY: configure_dev_all
configure_dev_all : configure_dev_heavy configure_dev_language_all

# General development
# Git for general version control.
# Tmux for easier multi-session development in a terminal.
# Vim as a fairly light-weight editor
.PHONY: configure_dev
configure_dev : link_bash link_git link_tmux configure_vim

# General development, but using Emacs as editor.
.PHONY: configure_dev_emacs
configure_dev_heavy : configure_emacs

# All language development
.PHONY: configure_dev_language_all
configure_dev_language_all : configure_dev_haskell configure_dev_idris \
	configure_dev_perl configure_dev_ruby

# Haskell development
.PHONY: configure_dev_haskell
configure_dev_haskell : configure_dev install_haskell_platform link_ghci

# Idris development
.PHONY: configure_dev_idris
configure_dev_idris : install_idris

# Perl development
.PHONY: configure_dev_perl
configure_dev_perl : install_cpanm

# Ruby development
.PHONY: configure_dev_ruby
configure_dev_ruby : install_ruby1.9.1 link_irb

# Enhanced development in Vim
# NOTE: We should also set up YCM here, but install is currently broken.
.PHONY: configure_vim
configure_vim : link_vim setup_vundle_plugins

# Misc scripts
.PHONY: configure_scripts
configure_scripts : link_scripts

# Misc tools
# Owncloud-desktop for file backup and syncing.
# Shutter for screenshots.
# Tmuxinator for Tmux session config.
.PHONY: configure_tools
configure_tools : install_owncloud_desktop install_shutter link_tmuxinator

# Better user experience (personal tools)
.PHONY: configure_user_all
configure_user_all : configure_tools configure_scripts configure_web

# Web use
# Icecat for browser.
# Vimperator for Vim-like bindings.
.PHONY: configure_web
configure_web : install_icecat link_vimperator

# Minimal

.PHONY: link_bash
link_bash :
	@$(call linkf,bash/.bash,.bash)
	@$(call linkf,bash/.profile,.profile)
	@$(call linkf,bash/.bash/.bashrc,.bashrc)

.PHONY: install_idris
install_idris :
	@$(call install_prog,idris)

.PHONY: install_vim
install_vim :
	$(call install_prog,vim)

.PHONY: link_vim
link_vim :
	$(call linkf,vim/.vimrc,.vimrc)
	@mkdir -p $(HOME)/.vim
	$(call linkf,vim/.vim/UltiSnips,.vim/UltiSnips)

.PHONY: install_git
install_git :
	$(call install_prog,git)

.PHONY: link_git
link_git : install_git
	$(call linkf,git/.gitconfig,.gitconfig)

.PHONY: link_scripts
link_scripts :
	$(call link_contents,bash/scripts,.local/bin)

# Medium

.PHONY: install_cpanm
install_cpanm :
	$(call install_prog,cpanm)

.PHONY: install_tmux
install_tmux :
	$(call install_prog,tmux)

.PHONY: link_tmux
link_tmux : install_tmux
	@$(call linkf,tmux/.tmux.conf,.tmux.conf)

vundle_dir = $(HOME)/.vim/bundle

.PHONY: install_vundle
install_vundle : install_git
	@mkdir $(vundle_dir) -p
	@[ -e "$(vundle_dir)/Vundle.vim" ] \
		 || git clone https://github.com/gmarik/Vundle.vim.git $(HOME)/.vim/bundle/Vundle.vim

# Full

.PHONY: install_icecat
install_icecat :
	@$(call install_prog,icecat)

.PHONY: link_vimerator
link_vimperator :
	$(call linkf,vim/.vimperatorrc,.vimperatorrc)

.PHONY: install_pip
install_pip :
	$(call install_prog,pip2)
	$(call install_prog,pip3)

.PHONY: install_mercurial
install_mercurial : install_pip
	@$(call install_prog,mercurial)

.PHONY: install_mu
install_mu :
	@$(call install_prog,mu)

# Use offlineimap for fetching mail
.PHONY: configure_mu
configure_mu : configure_offlineimap install_mu

.PHONY: install_offlineimap
install_offlineimap :
	@$(call install_prog,offlineimap)

.PHONY: link_offlineimap
link_offlineimap : install_offlineimap
	@$(call linkf,mail/.offlineimaprc,.offlineimaprc)

.PHONY: configure_offlineimap
configure_offlineimap : link_offlineimap

.PHONY: install_emacs
install_emacs :
	@$(call install_prog,emacs)

# Requires mercurial for 'evil'
# Requires mu for e-mail
# Requires Inconsolata font (used as font in Emacs)
.PHONY: configure_emacs
configure_emacs : configure_mu install_font_inconsolata install_mercurial link_emacs

.PHONY: link_emacs
link_emacs : install_emacs
	@$(call linkf,emacs/custom,.emacs.d/custom)
	@$(call linkf,emacs/custom/init.el,.emacs.d/init.el)
	@$(call linkf,emacs/custom/config.org,.emacs.d/config.org)

.PHONY : install_ghci
install_ghci :
	@[ $$(which ghci) ] || make install_haskell_platform

.PHONY: link_ghci
link_ghci : install_ghci
	$(call linkf,haskell/.ghci,.ghci)

.PHONY: install_haskell_platform
install_haskell_platform :
	  @$(call install_prog,haskell-platform)

.PHONY: install_owncloud_desktop
install_owncloud_desktop :
	@$(call install_prog,owncloud_desktop)

.PHONY: install_ruby1.9.1
install_ruby1.9.1 :
	$(call install_prog,ruby1.9.1)

.PHONY: install_tmuxinator
install_tmuxinator : install_ruby1.9.1
	$(call install_prog,tmuxinator)

.PHONY: link_tmuxinator
link_tmuxinator : install_tmuxinator
	@$(call linkf,tmux/.tmuxinator,.tmuxinator)

.PHONY: link_irb
link_irb : install_ruby1.9.1
	@$(call linkf,ruby/.irbrc,.irbrc)

.PHONY: install_shutter
install_shutter :
	@$(call install_prog,shutter)

# Other

.PHONY: setup_ycm
setup_ycm : install_vundle
	@$(call install_prog,cmake) \
		&& $(call install_prog,python-dev) \
		&& cd $(vundle_dir)/youcompleteme \
		&& ./install.py

.PHONY: setup_vundle_plugins
setup_vundle_plugins : install_vundle
	@vim -c 'exec "PluginInstall" | qa'

solarized_file_url = "https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark"
solarized_color_file = $(HOME)/.dircolors

.PHONY: get_solarized_colors
get_solarized_colors :
	    @[ -e $(solarized_color_file) ] \
				|| (echo "Retrieving solarized colorscheme" \
				&& wget -q $(solarized_file_url) -O $(solarized_color_file))

.PHONY: install_fontconfig
install_fontconfig :
	@$(call install_prog,fontconfig)

.PHONY: install_font_inconsolata
install_font_inconsolata :
	@$(call install_prog,font_inconsolata)

# Vim Linters

vim_linters = install_vim-vint

.PHONY: install_vim-vint
install_vim-vint :
	@pip3 install --user vim-vint
