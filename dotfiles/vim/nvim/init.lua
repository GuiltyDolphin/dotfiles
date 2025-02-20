-- Add ~/.vim to the runtime path
vim.opt.runtimepath:prepend("~/.vim")
vim.opt.runtimepath:append("~/.vim/after")

-- Source ~/.vimrc
vim.cmd('source ~/.vimrc')
