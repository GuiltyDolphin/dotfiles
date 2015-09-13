dot_dir = "$(PWD)/dotfiles"

.PHONY: install_vundle
install_vundle : install_git
	git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

.PHONY: install_git
install_git :
	[ $(which git) ] || apt-get install git

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
