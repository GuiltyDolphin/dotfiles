# Install a program via apt-get
install_prog = @[ $$(which $(1)) ] || (echo "Installing $(1)\n" \
							 &&  apt-get install $(1) -y)

dot_dir = $(PWD)/dotfiles

# Create a symlink if file doesn't already exist.
linkh = @$(call check_link,$(1),$(2)) || [ -e $(2) ] || ln -s $(1) $(2)
check_link = @([ ! -L "$(1)" -a -n "$$(diff -q $(2) $(1) 2>/dev/null)" ] \
						 || [ -L "$(2)" -a "$$(readlink -f $(2))" != "$(1)" ]) && echo "File $(2) exists but differs from $(1) - will not make symlink"
linkf = @$(call linkh,$(dot_dir)/$(1),$(HOME)/$(2))

# (link_contents,$(search_dir),$(target_dir))
# Create a link in $(target_dir) for every file in $(search_dir)
link_contents = @$(foreach fname, \
									$(wildcard $(dot_dir)/$(1)/*), \
										$(call linkh, \
											$(fname), \
											$(HOME)/$(2)/$(shell basename $(fname))))

links_minimal = link_bash link_git link_vim

links_medium = $(links_minimal) link_tmux

links_full = $(links_medium) link_emacs link_ghci link_tmuxinator link_vimperator link_irb

.PHONY: link
link : $(links_medium)

.PHONY: link_minimal
link_minimal : $(links_minimal)

.PHONY: link_medium
link_medium : $(links_medium)

.PHONY: link_full
link_full : $(links_full)

installs_haskell = install_haskell_platform install_ghci
installs_minimal = install_git install_vim
installs_medium = $(installs_minimal) install_tmux
installs_full = $(installs_medium) $(installs_haskell) install_ruby1.9.1 install_tmuxinator install_vundle

.PHONY: install
install : $(installs_medium)

.PHONY: install_minimal
install_minimal : $(installs_minimal)

.PHONY: install_medium
install_medium : $(installs_medium)

.PHONY: install_full
install_full : $(installs_full)

# Minimal

.PHONY: link_bash
link_bash :
	@$(call linkf,bash/.bash,.bash)
	@$(call linkf,bash/.profile,.profile)
	@$(call linkf,bash/.bash/.bashrc,.bashrc)

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

# Medium

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

.PHONY: link_vimerator
link_vimperator :
	$(call linkf,vim/.vimperatorrc,.vimperatorrc)

.PHONY: install_emacs
install_emacs :
	@(emacs --version | head -n 1 | egrep -q '24.5') \
	|| ((cd ~/Downloads && ([ -d emacs-24.5 ] || (wget \
	ftp://www.mirrorservice.org/sites/ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz \
	   && tar -xvf emacs-24.5)) && cd emacs-24.5 \
	   && ./configure && make && src/emacs -Q && make install))

.PHONY: link_emacs
link_emacs : install_emacs
	@$(call linkf,emacs/custom,.emacs.d/custom)
	@$(call linkf,emacs/custom/init.el,.emacs.d/init.el)

.PHONY : install_ghci
install_ghci :
	@[ $$(which ghci) ] || make install_haskell_platform

.PHONY: link_ghci
link_ghci : install_ghci
	$(call linkf,haskell/.ghci,.ghci)

.PHONY: install_haskell_platform
install_haskell_platform :
	@[ -n "$$(apt version haskell-platform)" ] \
	  || $(call install_prog,haskell-platform)

.PHONY: install_ruby1.9.1
install_ruby1.9.1 :
	$(call install_prog,ruby1.9.1)

.PHONY: install_tmuxinator
install_tmuxinator : install_ruby1.9.1
	@[ $$(which tmuxinator) ] || (echo "Installing tmuxinator" \
		&& gem1.9.1 install --user-install tmuxinator)

.PHONY: link_tmuxinator
link_tmuxinator : install_tmuxinator
	@$(call linkf,tmux/.tmuxinator,.tmuxinator)

.PHONY: link_irb
link_irb : install_ruby1.9.1
	@$(call linkf,ruby/.irbrc,.irbrc)

# Other

.PHONY: setup_ycm
setup_ycm : install_vundle
	@$(call install_prog,cmake) \
		&& $(call install_prog,python-dev) \
		&& cd $(vundle_dir)/youcompleteme \
		&& ./install.py

solarized_file_url = "https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark"
solarized_color_file = $(HOME)/.dircolors

.PHONY: get_solarized_colors
get_solarized_colors :
	    @[ -e $(solarized_color_file) ] \
				|| (echo "Retrieving solarized colorscheme" \
				&& wget -q $(solarized_file_url) -O $(solarized_color_file))

# Vim Linters

vim_linters = install_vim-vint

.PHONY: install_vim-vint
install_vim-vint :
	@pip3 install --user vim-vint
