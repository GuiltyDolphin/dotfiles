# Install a program via apt-get
install_prog = @[ $$(which $(1)) ] || (echo "Installing $(1)\n" \
							 &&  apt-get install $(1) -y)

# Create a symlink if file doesn't already exist.
linkf = @([ -e $(HOME)/$(2) ] \
	&& ([ $$(diff -q $(HOME)/$(2) $(dot_dir)/$(1)) ] \
		&& echo "File $(HOME)/$(2) exists but differs from $(dot_dir)/$(1) - will not make symlink") \
            || true) \
					|| ln -s $(dot_dir)/$(1) $(HOME)/$(2)

links = link_bash link_tmux link_tmuxinator link_vim

.PHONY: link
link : $(links)

installs = install_git install_ruby1.9.1 install_tmux install_tmuxinator install_vundle
.PHONY: install
install : $(installs)

solarized_file_url = "https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark"
solarized_color_file = $(HOME)/.dircolors
.PHONY: get_solarized_colors
get_solarized_colors :
	    @[ -e $(solarized_color_file) ] \
				|| (echo "Retrieving solarized colorscheme" \
				&& wget -q $(solarized_file_url) -O $(solarized_color_file))


dot_dir = $(PWD)/dotfiles

vundle_dir = $(HOME)/.vim/bundle

.PHONY: install_vundle
install_vundle : install_git
	@[ -d "$(vundle_dir)" ] || mkdir $(vundle_dir) -p
	@[ -e "$(vundle_dir)/Vundle.vim" ] \
		 || git clone https://github.com/gmarik/Vundle.vim.git $(HOME)/.vim/bundle/Vundle.vim


.PHONY: install_git
install_git :
	$(call install_prog,git)


.PHONY: link_vim
link_vim :
	$(call linkf,vim/.vimrc,.vimrc)
	$(call linkf,vim/.vimbasic,.vimbasic)


bash_dir = $(dot_dir)/bash

.PHONY: link_bash
link_bash :
	@$(call linkf,bash/.bash,.bash)
	@$(call linkf,bash/.profile,.profile)
	@$(call linkf,bash/.bash/.bashrc,.bashrc)


.PHONY: link_tmux
link_tmux : install_tmux
	@$(call linkf,tmux/.tmux.conf,.tmux.conf)


.PHONY: install_tmux
install_tmux :
	$(call install_prog,tmux)

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
