# Install a program via apt-get

debug = $(if $(MAKE_DEBUG),--debug)
installer = @perl install/install.pl $(debug) $(1)

install_prog = $(call installer,install) $(1)
linkf = $(call installer,link) $(1) $(2)
link_contents = $(call installer,link_contents) $(1) $(2)

##############################
#### Configuration Groups ####
##############################

.PHONY: configure_all \\
		bootstrap \\
		configure_dev_all \\
		configure_dev \\
		configure_dev_heavy \\
		configure_dev_emacs \\
		configure_dev_language_all \\
		configure_dev_common_lisp \\
		configure_dev_haskell \\
		configure_dev_idris \\
		configure_dev_java \\
		configure_dev_javascript \\
		configure_dev_perl \\
		configure_dev_ruby \\
		configure_vim \\
		configure_display \\
		configure_x_keyboard \\
		configure_scripts \\
		configure_tools \\
		configure_user_all \\
		configure_web

# Full Configuration
configure_all : bootstrap configure_dev_all configure_user_all

# Configure programs required (or recommended) for configuration of the installer
bootstrap : configure_recutils

# Full development configuration
configure_dev_all : configure_dev_heavy configure_dev_language_all

# General development
# Git for general version control.
# Tmux for easier multi-session development in a terminal.
# Vim as a fairly light-weight editor
configure_dev : link_bash configure_git link_tmux configure_vim

# General development, but using Emacs as editor.
configure_dev_heavy : configure_emacs

# All language development
configure_dev_language_all : \
	configure_dev_common_lisp \
	configure_dev_haskell \
	configure_dev_idris \
	configure_dev_java \
	configure_dev_javascript \
	configure_dev_perl \
	configure_dev_ruby

# Common Lisp development
configure_dev_common_lisp : configure_sbcl

# Haskell development
configure_dev_haskell : configure_dev install_haskell_platform link_ghci

# Idris development
configure_dev_idris : install_idris

# Java development
configure_dev_java : configure_eclim configure_emacs configure_jdk

# JavaScript development
configure_dev_javascript : configure_node

# Perl development
configure_dev_perl : install_cpanm

# Ruby development
configure_dev_ruby : install_ruby1.9.1 link_irb

# Enhanced development in Vim
# NOTE: We should also set up YCM here, but install is currently broken.
configure_vim : link_vim setup_vundle_plugins

# X config
configure_display : configure_urxvt configure_x_keyboard configure_xinit configure_xmonad \
	configure_xrdb configure_xscreensaver configure_xset link_xresources

# Keyboard (when using X)
configure_x_keyboard : install_setxkbmap

# Misc scripts
configure_scripts : link_scripts

# Misc tools
# freemind for mind mapping
# libreoffice as an office suite
# Owncloud-desktop for file backup and syncing.
# Shutter for screenshots.
# Tmuxinator for Tmux session config.
configure_tools : \
	configure_freemind \
	configure_libreoffice \
	install_owncloud_desktop \
	install_shutter \
	link_tmuxinator

# Better user experience (personal tools)
configure_user_all : configure_display configure_tools configure_scripts configure_web

# Web use
# Icecat for browser.
# Vimperator for Vim-like bindings.
configure_web : install_icecat link_vimperator

#############################
#### Individual Programs ####
#############################

.PHONY: configure_apache_ant
configure_apache_ant : install_apache_ant

.PHONY: install_apache_ant
install_apache_ant :
	$(call install_prog,apache_ant)

.PHONY: configure_aspell
configure_aspell : install_aspell

.PHONY: install_aspell
install_aspell :
	$(call install_prog,aspell)
	$(call install_prog,aspell-dict-en)

.PHONY: link_bash
link_bash :
	@$(call linkf,bash/.bash,.bash)
	@$(call linkf,bash/.profile,.profile)
	@$(call linkf,bash/.bash/.bashrc,.bashrc)

.PHONY: configure_cask
configure_cask : link_cask

.PHONY: install_cask
install_cask :
	@$(call install_prog,cask)

.PHONY: link_cask
link_cask : install_cask
	@$(call linkf,emacs/custom/Cask,.emacs.d/Cask)

.PHONY: install_cpanm
install_cpanm :
	$(call install_prog,cpanm)

.PHONY: configure_eclim
configure_eclim : configure_apache_ant configure_eclipse install_eclim

.PHONY: install_eclim
install_eclim :
	$(call install_prog,eclim)

.PHONY: configure_eclipse
configure_eclipse : install_eclipse

.PHONY: install_eclipse
install_eclipse :
	$(call install_prog,eclipse)

.PHONY: install_emacs
install_emacs :
	@$(call install_prog,emacs)

# Requires aspell for spell checking
# Requires cask for some dependency management
# Requires mercurial for 'evil'
# Requires mu for e-mail
# Requires Inconsolata font (used as font in Emacs)
.PHONY: configure_emacs
configure_emacs : configure_aspell configure_cask configure_mu install_font_inconsolata install_mercurial link_emacs

.PHONY: link_emacs
link_emacs : install_emacs
	@$(call linkf,emacs/custom,.emacs.d/custom)
	@$(call linkf,emacs/custom/init.el,.emacs.d/init.el)
	@$(call linkf,emacs/custom/config.org,.emacs.d/config.org)

.PHONY: configure_freemind
configure_freemind : link_freemind

.PHONY: link_freemind
link_freemind :
	@$(call linkf,freemind/patterns.xml,.freemind/patterns.xml)
	@$(call linkf,freemind/user.properties,.freemind/user.properties)

.PHONY: configure_gcc
configure_gcc : install_gcc

.PHONY: install_gcc
install_gcc :
	$(call install_prog,gcc)

.PHONY: configure_ghc7
configure_ghc7 : install_ghc7

.PHONY: install_ghc7
install_ghc7 :
	$(call install_prog,ghc7)

.PHONY : install_ghci
install_ghci :
	@[ $$(which ghci) ] || make install_haskell_platform

.PHONY: link_ghci
link_ghci : install_ghci
	$(call linkf,haskell/.ghci,.ghci)

.PHONY: configure_git
configure_git : link_git configure_git_autocomplete

.PHONY: install_git
install_git :
	$(call install_prog,git)

.PHONY: link_git
link_git : install_git
	$(call linkf,git/.gitconfig,.gitconfig)

git_completion_target=~/.git-completion.bash
.PHONY: configure_git_autocomplete
configure_git_autocomplete :
	@if [ ! -f $(git_completion_target) ]; then \
	echo "Downloading git completion file" \
		&& curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -o $(git_completion_target); fi

.PHONY: configure_glibc
configure_glibc : install_glibc

.PHONY: install_glibc
install_glibc :
	$(call install_prog,glibc)

.PHONY: install_haskell_platform
install_haskell_platform :
	  @$(call install_prog,haskell-platform)

.PHONY: install_icecat
install_icecat :
	@$(call install_prog,icecat)

.PHONY: install_icedtea_jdk
install_icedtea_jdk :
	$(call install_prog,icedtea_jdk)

.PHONY: install_idris
install_idris :
	@$(call install_prog,idris)

.PHONY: link_irb
link_irb : install_ruby1.9.1
	@$(call linkf,ruby/.irbrc,.irbrc)

.PHONY: configure_jdk
configure_jdk : install_icedtea_jdk

.PHONY: configure_libreoffice
configure_libreoffice : install_libreoffice

.PHONY: install_libreoffice
install_libreoffice :
	@$(call install_prog,libreoffice)

.PHONY: install_mercurial
install_mercurial : install_pip
	@$(call install_prog,mercurial)

.PHONY: install_mu
install_mu :
	@$(call install_prog,mu)

# Use offlineimap for fetching mail
.PHONY: configure_mu
configure_mu : configure_offlineimap install_mu

.PHONY: configure_node
configure_node : install_node link_npm

.PHONY: install_node
install_node :
	@$(call install_prog,node)

.PHONY: link_npm
link_npm :
	@$(call linkf,node/.npmrc,.npmrc)

.PHONY: install_offlineimap
install_offlineimap :
	@$(call install_prog,offlineimap)

.PHONY: link_offlineimap
link_offlineimap : install_offlineimap
	@$(call linkf,mail/.offlineimaprc,.offlineimaprc)

.PHONY: configure_offlineimap
configure_offlineimap : link_offlineimap

.PHONY: install_owncloud_desktop
install_owncloud_desktop :
	@$(call install_prog,owncloud_desktop)

.PHONY: install_pip
install_pip :
	$(call install_prog,pip2)
	$(call install_prog,pip3)

.PHONY: configure_recutils
configure_recutils : install_recutils

.PHONY: install_recutils
install_recutils :
	$(call install_prog,recutils)

.PHONY: configure_rofi
configure_rofi : install_rofi link_xresources

.PHONY: install_rofi
install_rofi :
	$(call install_prog,rofi)

.PHONY: install_ruby1.9.1
install_ruby1.9.1 :
	$(call install_prog,ruby1.9.1)

.PHONY: configure_sbcl
configure_sbcl : install_sbcl

.PHONY: install_sbcl
install_sbcl :
	$(call install_prog,sbcl)

.PHONY: link_scripts
link_scripts :
	$(call link_contents,bash/scripts,.local/bin)
	$(call link_contents,scripts,.local/bin)

.PHONY: install_setxkbmap
setxkbmap :
	@$(call install_prog,setxkbmap)

.PHONY: install_shutter
install_shutter :
	@$(call install_prog,shutter)

.PHONY: install_tmux
install_tmux :
	$(call install_prog,tmux)

.PHONY: link_tmux
link_tmux : install_tmux
	@$(call linkf,tmux/.tmux.conf,.tmux.conf)

.PHONY: install_tmuxinator
install_tmuxinator : install_ruby1.9.1
	$(call install_prog,tmuxinator)

.PHONY: link_tmuxinator
link_tmuxinator : install_tmuxinator
	@$(call linkf,tmux/.tmuxinator,.tmuxinator)

# Inconsolata is preferred terminal font.
.PHONY: configure_urxvt
configure_urxvt : install_font_inconsolata install_urxvt

.PHONY : install_urxvt
install_urxvt :
	$(call install_prog,urxvt)

.PHONY: install_vim
install_vim :
	$(call install_prog,vim)

.PHONY: link_vim
link_vim :
	$(call linkf,vim/.vimrc,.vimrc)
	@mkdir -p $(HOME)/.vim
	$(call linkf,vim/.vim/UltiSnips,.vim/UltiSnips)

.PHONY: link_vimperator
link_vimperator :
	$(call linkf,vim/.vimperatorrc,.vimperatorrc)

vundle_dir = $(HOME)/.vim/bundle

.PHONY: install_vundle
install_vundle : install_git
	@mkdir $(vundle_dir) -p
	@[ -e "$(vundle_dir)/Vundle.vim" ] \
		 || git clone https://github.com/gmarik/Vundle.vim.git $(HOME)/.vim/bundle/Vundle.vim

.PHONY: configure_xinit
configure_xinit : link_xinit

.PHONY: install_xinit
install_xinit :
	$(call install_prog,xinit)

.PHONY: link_xinit
link_xinit : install_xinit
	$(call linkf,x/.xserverrc,.xserverrc)
	$(call linkf,x/.xinitrc,.xinitrc)

.PHONY: configure_xmobar
configure_xmobar : link_xmobar

.PHONY: install_xmobar
install_xmobar :
	$(call install_prog,xmobar)

.PHONY: link_xmobar
link_xmobar : install_xmobar
	$(call linkf,x/.xmobarrc,.xmobarrc)

.PHONY: configure_xmonad
configure_xmonad : configure_rofi link_xmonad

.PHONY: install_xmonad
install_xmonad : configure_gcc configure_glibc configure_xmobar
	$(call install_prog,xmonad)
	$(call install_prog,xmonad_contrib)

.PHONY: link_xmonad
link_xmonad : install_xmonad
	$(call linkf,x/.xmonad/xmonad.hs,.xmonad/xmonad.hs)

.PHONY: configure_xrdb
configure_xrdb : install_xrdb

.PHONY: install_xrdb
install_xrdb :
	$(call install_prog,xrdb)

.PHONY: link_xresources
link_xresources :
	$(call linkf,x/.Xresources,.Xresources)

.PHONY: configure_xscreensaver
configure_xscreensaver : link_xscreensaver

.PHONY: install_xscreensaver
install_xscreensaver :
	$(call install_prog,xscreensaver)

.PHONY: link_xscreensaver
link_xscreensaver : install_xscreensaver
	$(call linkf,x/.xscreensaver,.xscreensaver)

.PHONY: configure_xset
configure_xset : install_xset

.PHONY: install_xset
install_xset :
	$(call install_prog,xset)

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
