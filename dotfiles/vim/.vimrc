" Vim config file
" GuiltyDolphin (Ben Moon)

" Vundle {{{
set nocompatible              " be iMproved
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'tex.vim'
Plugin 'altercation/vim-colors-solarized' " Solarized colorscheme
Plugin 'bitc/vim-hdevtools'
Plugin 'dbakker/vim-lint'
Plugin 'eagletmt/neco-ghc'
Plugin 'ehamberg/vim-cute-python'
Plugin 'honza/vim-snippets.git'    " Default ultisnips snippets
Plugin 'jpalardy/vim-slime'        " One way communication to a (tmux) session
Plugin 'kien/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'pbrisbin/vim-syntax-shakespeare'  " Highlighting for Yesod Shakespeare
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'      " Syntax error checking
Plugin 'sirver/ultisnips'          " Snippet integration
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'        " Git integration
Plugin 'tpope/vim-surround'        " Allow manipulation of surrounding characters
Plugin 'Twinside/vim-haskellConceal'
Plugin 'valloric/youcompleteme'    " Code completion
Plugin 'vim-ruby/vim-ruby'         " Convinient bindings for Ruby
Plugin 'lesliev/vim-inform7'       " Syntax file highlighting for inform


call vundle#end()            " required
filetype plugin indent on    " required
" }}}

" Colorscheme {{{
syntax on
set t_Co=256
set background=dark
colorscheme solarized
" }}}

" Custom mappings {{{

" Space as leader.
map <space> <leader>
" Comma as local leader.
map , <localleader>

" Move in actual lines rather than virtual
nnoremap <silent> k gk
nnoremap <silent> j gj

" Open .vimrc
nnoremap <silent> <leader>ev :vsplit $MYVIMRC<cr>
" Source .vimrc
nnoremap <silent> <leader>sv :source $MYVIMRC<cr>

" Use ; to access command-line
nnoremap ; :
nnoremap : ;
xnoremap ; :
xnoremap : ;

" Faster saving
nnoremap <silent> <leader>w :write<cr>

" Turn off highlighting for last search
nnoremap <silent> <leader>cs :nohlsearch<CR>

" Toggle spell-checking for current buffer
nnoremap <silent> <leader>cS :setlocal spell!<CR>

" Avoid tmux conflict
nnoremap <silent> <c-n> <nop>

" Plugins {{{

" Toggle tagbar window
nnoremap <silent> <leader>b :TagbarToggle<cr>
" Open ctrlp tag window
nnoremap <silent> <leader>p :CtrlPTag<cr>
" Toggle NERDTree window
nnoremap <silent> <leader>n :NERDTreeToggle<cr>

" Ultisnips
let g:UltiSnipsJumpForwardTrigger = "<c-b>"
let g:UltiSnipsExpandTrigger = "<c-b>"

" fugitive {{{
nnoremap <silent> <leader>ga :Git add %<cr>
nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gd :Git diff %<cr>
" }}}
" }}}

" Filetypes {{{
augroup Haskell
  autocmd!
  au FileType haskell nnoremap <buffer> <localleader>ct :HdevtoolsType<cr>
  au FileType haskell nnoremap <buffer> <localleader>cc :HdevtoolsClear<cr>
  au FileType haskell nnoremap <buffer> <localleader>ci :HdevtoolsInfo<cr>
augroup END

augroup Ruby
  autocmd!
  au FileType ruby nnoremap <silent><buffer> <localleader>rt :!rake test<cr>
augroup END
" }}}
" }}}

" Options {{{

" Boolean options {{{

" Line numbers
set number         " Display line numbers on left
set relativenumber " Display line numbers relative to current
" Indenting
set autoindent     " Keep indentation of previous line
set copyindent     " Copy indent character used for previous line
set shiftround     " Round autoindenting to a multiple of 'shiftwidth'
set expandtab      " Expand tabs to spaces
set smarttab       " Insert tabs according to shiftwidth
" Searching
set ignorecase     " Ignore cases in searches
set smartcase      " Override 'ignorecase' if pattern contains upper case characters
set hlsearch       " Highlight matches for last search
set incsearch      " Highlight next match

set hidden         " Hide buffers instead of closing
set nowrap         " Don't show lines wrapping in the buffer
set noerrorbells   " Don't use alerts for errors that have messages
" Backups
set noswapfile     " Don't use swap files for the current buffer
set nobackup       " Don't make backup copies of files when overwriting
"set notimeout     " Allow umlimited time to enter symbols (mathematic.vim)
" }}}

" Number option {{{
" Indenting
set laststatus=2 " Shows the statusline.
set shiftwidth=2 " Generally use 2 spaces for indenting
set softtabstop=2
set tabstop=2
" }}}

" String options {{{
" Spelling
set dictionary+=/usr/share/dict/words
set dictionary-=/usr/share/dict/words " Avoid duplicate words
set spelllang=en_us " Language to use when spell-checking
set backspace=indent,eol,start        " Use backspace jumping in insert mode

" Statusline {{{
set statusline=                 " clear stl for when reloaded
set statusline+=%f\             " Path to the file
set statusline+=[%h%m%r%w]\ -\  " Flags
set statusline+=FileType:       " Label
set statusline+=%y              " Filetype of the file
set statusline+=%=              " Switch to the right side
set statusline+=(%l,\ %c)       " Current line, current column
set statusline+=/               " Separator
set statusline+=%-L             " Total # lines
set statusline+=%4p             " Percentage through file (lines)
" }}}

" }}}

" Variable {{{
" Plugins {{{
" Syntastic
let g:syntastic_python_checkers = ["python", "pep8", "pylint"]
let g:syntastic_python_pylint_quiet_messages = { "regex": "parens after u'print'" }
let g:syntastic_ruby_checkers = ["mri", "rubylint", "rubocop"]
let g:syntastic_haskell_checkers = ["hdevtools", "hlint"]
let g:syntastic_enable_perl_checker = 1
let g:syntastic_perl_checkers = ["perl", "podchecker"]
let g:syntastic_haskell_hdevtools_quiet_messages = { "regex": 'Could not find module' }
" vim-slime
let g:slime_target = "tmux"
let g:slime_paste_file = tempname()
" YouCompleteMe
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:ycm_add_preview_to_completeopt = 0
let g:ycm_autoclose_preview_window_after_insertion = 1 " Autoclose the info window popup when leaving insert mode
" }}}
" haskellmode {{{
let g:haddock_browser = "/usr/bin/lynx"
let g:ghc = "/usr/bin/ghc"
" }}}
" }}}
" }}}

" Plugin Config {{{
" haskellmode {{{
"augroup HaskellMode
"  autocmd!
"  au BufEnter *.hs compiler ghc
"augroup END
" }}}
" }}}

" Autocommands {{{

" Delete trailing whitespace when saving
augroup writer
    autocmd!
    autocmd BufWritePre * :%s/\v\s+$//e
augroup END

" Automatically jump to (and center on) last place in file
augroup newfile
    autocmd!
    autocmd BufRead * execute 'normal!' . "'" . '"zz'
augroup END

" Python {{{
augroup Python
  autocmd!
  autocmd FileType python nnoremap <buffer> <localleader>ra :call <SID>PythonTestAll()<CR>
  autocmd FileType python set shiftwidth=4
augroup END

function! s:PythonTestAll()
  let test_file = expand("test_") . expand("%")
  if filereadable("setup.py")
    execute "!python3 setup.py test"
  elseif filereadable("../setup.py")
    execute "!cd ..;python3 setup.py test;cd -"
  else
    echo "No tests found"
  endif
endfunction
" }}}
" Vim {{{
augroup Vim
  autocmd!
  au FileType vim :setlocal foldmethod=marker
augroup END
" }}}
" Haskell {{{
augroup Haskell
  autocmd!
  au FileType haskell setlocal omnifunc=necoghc#omnifunc
  au FileType haskell let g:ycm_semantic_triggers={'haskell' : ['.', '= ', '> ', '- ', ':: '] }
augroup END
" }}}
" Inform {{{
augroup inform7
  autocmd!
  au FileType inform7 setlocal noexpandtab
augroup END
" }}}
" }}}

