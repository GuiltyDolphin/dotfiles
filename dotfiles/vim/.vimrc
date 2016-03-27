" Vim config file
" Part of GuiltyDolphin's dotfiles
" Hosted at: https://www.github.com/GuiltyDolphin/config

set nocompatible              " be iMproved


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
Plugin 'mustache/vim-mustache-handlebars' " Mustache & Handlebars integration

Plugin 'guiltydolphin/tex-headings-vim' " Who doesn't love to change section headers?
Plugin 'guiltydolphin/project-root-vim' " Easier project navigation.

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

" Standard {{{

" Space as leader.
let mapleader = "\<Space>"
nnoremap <Space> <nop>
" Comma as local leader.
let maplocalleader = ','
nnoremap , <nop>

" Provide alternate methods of reversing find.
nnoremap \ ,
nnoremap ,, ,

" Move in actual lines rather than virtual
nnoremap <silent> k gk
nnoremap <silent> j gj

" Jump to parens
nnoremap gp %

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

" }}}

" Plugins {{{

" Toggle tagbar window
nnoremap <silent> <leader>b :TagbarToggle<cr>
" Open ctrlp tag window
nnoremap <silent> <leader>p :CtrlP<cr>
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

" project-root {{{

nnoremap <silent> <localleader>ra :ProjectRootTest<cr>
nnoremap <silent> <localleader>rt :ProjectRootTestFile<cr>
nnoremap <silent> <localleader>br :ProjectRootBrowseRoot<cr>
nnoremap <silent> <localleader>tf :ProjectRootOpenTest<cr>

" }}}

" }}}

" Filetypes {{{

" Haskell {{{

augroup HaskellKeys
  autocmd!
  au FileType haskell nnoremap <buffer> <localleader>ct :HdevtoolsType<cr>
  au FileType haskell nnoremap <buffer> <localleader>cc :HdevtoolsClear<cr>
  au FileType haskell nnoremap <buffer> <localleader>ci :HdevtoolsInfo<cr>
augroup END

" }}}

" Ruby {{{

augroup RubyKeys
  autocmd!
  au FileType ruby nnoremap <buffer> <localleader>ir :exec "!irb" . ruby_version<cr>
augroup END

" }}}

" Git {{{

augroup GitCommitKeys
  autocmd!
  au FileType gitrebase nnoremap <silent><buffer> <localleader>r ^ciwreword<esc>
  au FileType gitrebase nnoremap <silent><buffer> <localleader>s ^ciwsquash<esc>
  au FileType gitrebase nnoremap <silent><buffer> <localleader>e ^ciwedit<esc>
  au FileType gitrebase nnoremap <silent><buffer> <localleader>p ^ciwpick<esc>
  au FileType gitrebase nnoremap <silent><buffer> <localleader>f ^ciwfixup<esc>
  au FileType gitrebase nnoremap <silent><buffer> <localleader>x ^ciwexec<esc>
augroup END

" }}}

" Help {{{

augroup HelpKeys
  au!
  au FileType help,text nnoremap <silent><buffer> <localleader>th :call <SID>ToggleHelpType()<cr>
augroup END

function! s:ToggleHelpType()
  if &filetype =~ '\vt[e]xt'
    setlocal filetype=help
  elseif &filetype =~ 'help'
    setlocal filetype=text
  endif
endfunction

" }}}

" Vim {{{

augroup VimKeys
  autocmd!
  au FileType vim nnoremap <silent><buffer> <localleader>sf :source %<cr>
augroup END

" }}}

" TeX {{{

augroup GenericTeXKeys
  autocmd!
  au FileType lhaskell,tex,plaintex nnoremap <silent><buffer> <localleader>hh :call TeXHeaderHigher()<cr>
  au FileType lhaskell,tex,plaintex nnoremap <silent><buffer> <localleader>hl :call TeXHeaderLower()<cr>
augroup END

" }}}

" }}}

" }}}

" Abbreviations {{{

cnorea fl function-list

" }}}

" Options {{{

" Boolean options {{{

" Line numbers
set number         " Display line numbers on left
set relativenumber " Display line numbers relative to current

" Indenting {{{

set autoindent     " Keep indentation of previous line
set copyindent     " Copy indent character used for previous line
set shiftround     " Round autoindenting to a multiple of 'shiftwidth'
set expandtab      " Expand tabs to spaces
set smarttab       " Insert tabs according to shiftwidth

" }}}

" Searching {{{

set ignorecase     " Ignore cases in searches
set smartcase      " Override 'ignorecase' if pattern contains upper case characters
set hlsearch       " Highlight matches for last search
set incsearch      " Highlight next match

" }}}

set hidden         " Hide buffers instead of closing
set nowrap         " Don't show lines wrapping in the buffer
set noerrorbells   " Don't use alerts for errors that have messages
" Backups
set noswapfile     " Don't use swap files for the current buffer
set nobackup       " Don't make backup copies of files when overwriting
"set notimeout     " Allow umlimited time to enter symbols (mathematic.vim)

" }}}

" Number options {{{

" Indenting {{{

set laststatus=2 " Shows the statusline.
set shiftwidth=2 " Generally use 2 spaces for indenting
set softtabstop=2
set tabstop=2

" }}}

" Bash {{{

let g:is_bash = 1 " Default to using bash as filetype.
let g:sh_fold_enabled = 5 " Enable function & if/do/for folding.

" }}}

" }}}

" String options {{{

" Spelling {{{

set dictionary+=/usr/share/dict/words
set dictionary-=/usr/share/dict/words " Avoid duplicate words
set spelllang=en_us " Language to use when spell-checking

" }}}

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

" Syntastic {{{

let g:syntastic_python_checkers = ["python", "pep8", "pylint"]
let g:syntastic_python_pylint_quiet_messages = {
            \ "regex": "\\(parens after u'print'"
            \ . "\\|Redefining built-in 'file'\\)" }
let g:syntastic_ruby_checkers = ["mri", "rubocop", "rubylint"]
let g:syntastic_haskell_checkers = ["hdevtools", "hlint"]
let g:syntastic_enable_perl_checker = 1
let g:syntastic_perl_checkers = ["perl", "podchecker"]
let g:syntastic_haskell_hdevtools_quiet_messages = { "regex": 'Could not find module' }
let g:syntastic_javascript_checkers = ["jshint", "jslint"]
let g:syntastic_javascript_jshint_quiet_messages = { "regex": "\v'(DDH|$)' was used before it was defined." }
let g:syntastic_javascript_jslint_quiet_messages = { "regex": "\v'(DDH|$)' was used before it was defined." }
let g:syntastic_vim_checkers = ['vint']

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

" FileTypes {{{

" Ruby {{{

let ruby_version = "2.0" " Preferred ruby version

" }}}

" }}}

" }}}

" Plugin Config {{{

" project-root-vim {{{

" Project settings

" Ruby {{{

let s:pr_ruby =
      \ { 'root_globs': ['[Rr]akefile'],
      \   'test_command': 'rake test',
      \ }

call proot#initialize_project('ruby', s:pr_ruby)

function! s:RubySpecTestFile(root_dir)
  return 'spec/' . fnamemodify(a:root_dir, ':t:r') . '_spec.rb'
endfunction

function! s:RubySpecTestFileCommand(test_path)
  return 'ruby ' . a:test_path
endfunction

let s:pr_ruby_spec =
      \ { 'test_globs': ['spec'],
      \   'inherits': ['ruby'],
      \   'test_file_gen': function('s:RubySpecTestFile'),
      \   'test_command_file': function('s:RubySpecTestFileCommand'),
      \ }

call proot#initialize_project('ruby_spec', s:pr_ruby_spec)

" Set the ruby project type based on the type of tests used.
function! s:SetRubyProject()
  let spec_match = globpath(b:project_root_directory, 'spec')
  if !empty(spec_match)
    call proot#set_project_type('ruby_spec')
  endif
endfunction

call proot#add_project_runners('ruby', [function('s:SetRubyProject')])

" }}}

" Python {{{

function! s:PythonTestFile(root_dir)
  return 'tests/test_' . fnamemodify(a:root_dir, ':t')
endfunction

function! s:PythonTestFileCommand(test_path)
  let test_module = 'tests.' . fnamemodify(a:test_path, ':t:r')
  return '[ -e "setup.py" ] && python3 setup.py test -s ' . test_module . ' || '
        \ . 'PYTHONPATH=$PYTHONPATH:./src python3 -m ' . test_module
endfunction

let s:pr_python =
      \ { 'root_globs': ['setup.py', 'src'],
      \   'test_command': 'python3 setup.py test',
      \   'test_file_gen': function('s:PythonTestFile'),
      \   'test_command_file': function('s:PythonTestFileCommand'),
      \ }

call proot#initialize_project('python', s:pr_python)

" }}}

" Haskell {{{

let s:pr_haskell =
      \ { 'root_globs': ['*.cabal'],
      \   'test_command': 'cabal test',
      \ }

call proot#initialize_project('haskell', s:pr_haskell)

" }}}

" Perl {{{

function! s:PerlTestFile(root_dir)
  return 't/' . fnamemodify(a:root_dir, ':t:r') . '.t'
endfunction

function! s:PerlTestFileCommand(test_path)
  return 'prove -Ilib ' . a:test_path
endfunction

let s:pr_perl =
      \ { 'root_globs': ['lib', 't'],
      \   'test_command': 'prove -Ilib',
      \   'test_file_gen': function('s:PerlTestFile'),
      \   'test_command_file': function('s:PerlTestFileCommand'),
      \ }

call proot#initialize_project('perl', s:pr_perl)

" DuckDuckGo {{{

function! s:SetPerlProject()
  setlocal shiftwidth=4
  let a:dir = fnamemodify(b:project_root_directory, ":t")
  if a:dir =~ '\v(duckduckgo|p5-app-duckpan)'
    call proot#set_project_type('ddg_backend')
  elseif a:dir =~ '\v^(zeroclickinfo-.*)'
    call proot#set_project_type('ddg_zci')
  endif
endfunction

call proot#add_project_runners('perl', [function('s:SetPerlProject')])

let s:pr_ddg_backend = { 'inherits': ['perl'] }

call proot#initialize_project('ddg_backend', s:pr_ddg_backend)

function! s:SetDDGBackendProject()
  setlocal noexpandtab
  setlocal shiftwidth=2
endfunction

call proot#add_project_runners('ddg_backend', [function('s:SetDDGBackendProject')])

function! s:DDGZCITestFile(root_dir)
  if a:root_dir =~# '.*Role\/.*'
    return 't/00-roles.t'
  elseif a:root_dir =~# '.*share.*'
    return matchstr(a:root_dir, '\v.*share/\zs([_\w]+)\ze/?.*')
  endif
  return 't/' . fnamemodify(a:root_dir, ':t:r') . '.t'
endfunction

function! s:DDGZCITestFileCommand(test_path)
  if a:test_path ==# 't/00-roles.t'
    return 'prove -Ilib t/00-roles.t'
  endif
  let ia_name = fnamemodify(a:test_path, ':t:r')
  return 'duckpan test ' . ia_name
endfunction

let s:pr_ddg_zci =
      \ { 'inherits': ['perl'],
      \   'test_command': 'duckpan test',
      \   'test_file_gen': function('s:DDGZCITestFile'),
      \   'test_command_file': function('s:DDGZCITestFileCommand'),
      \ }

call proot#initialize_project('ddg_zci', s:pr_ddg_zci)

" }}}

" }}}

" }}}

" }}}

" Autocommands {{{

" Buffer-based {{{

" Whitespace {{{

" Delete trailing whitespace when saving
augroup writer
    autocmd!
    autocmd BufWritePre * call <SID>RemoveTrailingWhitespace()
augroup END

function! s:RemoveTrailingWhitespace()
  if !exists('b:allow_trailing_whitespace')
    exec '%s/\v\s+$//e'
  endif
endfunction

" }}}

" Automatically jump to (and center on) last place in file
augroup newfile
    autocmd!
    autocmd BufRead * execute 'normal!' . "'" . '"zz'
augroup END

" }}}

" Bash {{{

augroup bash
  autocmd!
  autocmd FileType bash :setlocal foldmethod=syntax
augroup END

" }}}

" Inform {{{

augroup inform7
  autocmd!
  au FileType inform7 setlocal noexpandtab
augroup END

" }}}

" JavaScript {{{

augroup JavaScript
  au!
  au FileType javascript setlocal shiftwidth=4
augroup END

" }}}

" JSON {{{

augroup JSON
  au!
  au FileType json setlocal shiftwidth=4
augroup END

" }}}

" Python {{{

augroup Python
  autocmd!
  autocmd FileType python setlocal shiftwidth=4
augroup END

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

" Git {{{

augroup gitdiff
  au!
  au FileType diff let b:allow_trailing_whitespace = 1
augroup END

" }}}

" Spelling {{{

augroup SpellCheck
  au!
  au FileType gitrebase,gitcommit,bib,tex,plaintex,lhaskell,*markdown*
        \ setlocal spell
augroup END

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

" }}}

" Custom functions {{{

" Haskell {{{

" Attempts to generate a suitable Haskell module name for the
" current file.
"
" If the file is part of a cabal project (i.e there exists a .cabal file)
" then the name can be generated intelligently, otherwise the basename
" of the file (without the extension) will be used.
"
" Example:
" If the project is /stuff/cabal-dir/src/Foo/Bar/Baz.hs then the generate
" module name will be Foo.Bar.Baz
"
" If there were no directory containing a .cabal file, then the generated
" module name would be Baz.
function! HaskellModuleName()
 let cabal_path = b:project_root_directory
 if expand("%") =~ '\v^$'
   echoerr "Not in a valid module file"
   return -1
 endif
 if cabal_path =~ '\v^$'
   return expand("%:t:r")
 endif
 let full_path = expand("%:p:r")
 let below_cabal = substitute(full_path, cabal_path, "", "")
 let module_path = matchlist(below_cabal, '\v/[^/]+/(.*)')[1]
 return substitute(module_path, '/', '.', 'g')
endfunction

" }}}

" }}}
