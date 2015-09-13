dot_dir = "$(PWD)/dotfiles"


.PHONY: install_vundle
install_vundle : install_git
	git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim


.PHONY: install_git
install_git :
	[ $(which git) ] || apt-get install git -y


links = link_vim link_bash

.PHONY: link_vim
link_vim :
	ln -s "$(dot_dir)/vim/.vimrc" "$(HOME)/.vimrc"
	ln -s "$(dot_dir)/vim/.vimbasic" "$(HOME)/.vimbasic"


bash_dir = "$(dot_dir)/bash"

.PHONY: link_bash
link_bash :
	ln -s "$(bash_dir)/.bash" "$(HOME)/.bash"
	ln -s "$(bash_dir)/.profile" "$(HOME)/.profile"
	ln -s "$(bash_dir)/.bash/.bashrc" "$(HOME)/.bashrc"


.PHONY: link_tmux
link_tmux : link_tmux
	ln -s "$(bash_dir)/tmux/.tmux.conf" "$(HOME)/.tmux.conf"


.PHONY: install_tmux
install_tmux :
	[ $(which tmux) ] || apt-get install tmux -y

.PHONY: install_ruby1.9.1
install_ruby1.9.1 :
	[ $(which ruby1.9.1) ] || apt-get install ruby1.9.1

.PHONY: install_tmuxinator
install_tmuxinator : install_ruby1.9.1
	[ $(which tmuxinator) ] || gem1.9.1 install --user-install tmuxinator

.PHONY: link_tmuxinator
link_tmuxinator : install_tmuxinator
	ln -s "$(dot_dir)/tmux/.tmuxinator" "$(HOME)/.tmuxinator"

.PHONY: get_solarized_colors
get_solarized_colors :
	    file_url = "https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark"
	    color_dir = "$(HOME)/.dir_colors"
	    [ -d $(color_dir) ] || wget -q $(file_url) -O $(color_dir)


