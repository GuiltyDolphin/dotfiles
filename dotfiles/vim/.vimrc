" Vim config file
" GuiltyDolphin (Ben Moon)

" Source script with basic definitions
source ~/.vim/.vimbasic

" Vundle {{{
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'


" Disable to stop Vundle from using syntax plugins.
let g:use_syntax_plugins = 1

if g:use_syntax_plugins
  Plugin 'lervag/vimtex'
  Plugin 'lesliev/vim-inform7'       " Syntax file highlighting for inform
  Plugin 'vim-ruby/vim-ruby'         " Convinient bindings for Ruby
  Plugin 'Twinside/vim-haskellConceal'
  " Plugin 'pbrisbin/vim-syntax-shakespeare'  " Highlighting for Yesod Shakespeare
  Plugin 'ehamberg/vim-cute-python'
endif

" Disable to stop large plugins being used (>10M)
let g:use_big_plugins = 1

if g:use_big_plugins
  Plugin 'valloric/youcompleteme'    " Code completion
endif


Plugin 'altercation/vim-colors-solarized' " Solarized colorscheme
Plugin 'bitc/vim-hdevtools'
Plugin 'dbakker/vim-lint'
Plugin 'eagletmt/neco-ghc'
Plugin 'honza/vim-snippets.git'    " Default ultisnips snippets
Plugin 'jpalardy/vim-slime'        " One way communication to a tmux session
Plugin 'kien/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'      " Syntax error checking
Plugin 'sirver/ultisnips'          " Snippet integration
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'        " Git integration
Plugin 'tpope/vim-surround'        " Allow manipulation of surrounding characters
Plugin 'godlygeek/tabular'         " Text filtering and alignment
Plugin 'plasticboy/vim-markdown'   " Must come after 'godlygeek/tabular'


call vundle#end()            " required
filetype plugin indent on    " required
" }}}

" Colorscheme {{{
colorscheme solarized
" }}}

" Custom mappings {{{

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
" Variable {{{
" Plugins {{{
" Syntastic {{{
let g:syntastic_python_checkers = ["python", "pep8", "pylint"]
let g:syntastic_python_pylint_quiet_messages = { "regex": "parens after u'print'" }
let g:syntastic_ruby_checkers = ["mri", "rubylint", "rubocop"]
let g:syntastic_haskell_checkers = ["hdevtools", "hlint"]
let g:syntastic_enable_perl_checker = 1
let g:syntastic_perl_checkers = ["perl", "podchecker"]
let g:syntastic_haskell_hdevtools_quiet_messages = { "regex": 'Could not find module' }
" }}}
" vim-slime {{{
let g:slime_target = "tmux"
let g:slime_paste_file = tempname()
" }}}
" YouCompleteMe {{{
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
" Haskell {{{
augroup Haskell
  autocmd!
  au FileType haskell setlocal omnifunc=necoghc#omnifunc
  au FileType haskell let g:ycm_semantic_triggers={'haskell' : ['.', '= ', '> ', '- ', ':: '] }
augroup END
" }}}
" }}}

" Hasktags (tagbar) {{{
let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
    \ }
" }}}
