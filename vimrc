set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles here:
"
" original repos on github
Bundle 'zhaocai/GoldenView.Vim'
Bundle 'kien/ctrlp.vim'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'mattn/gist-vim'
Bundle 'gregsexton/gitv'
Bundle 'sjl/gundo.vim'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'vim-scripts/hgrev'
Bundle 'bitc/lushtags'
Bundle 'ujihisa/neco-ghc'
Bundle 'Shougo/neocomplcache.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/syntastic'
Bundle 'majutsushi/tagbar'
Bundle 'flazz/vim-colorschemes'
Bundle 'skammer/vim-css-color'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-git'
Bundle 'airblade/vim-gitgutter'
Bundle 'Twinside/vim-haskellConceal'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'mattdenner/vim-scala'
Bundle 'dag/vim2hs'
Bundle 'Shougo/vimproc.vim'

" vim-scripts repos
Bundle 'a.vim'

" non github repos
" Bundle 'git://git.wincent.com/command-t.git'
" git repos on your local machine (ie. when working on your own plugin)
" Bundle 'file:///Users/gmarik/path/to/plugin'
" ...

filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..
"

syntax on
filetype on
filetype plugin indent on

set hidden
set textwidth=80
set autoindent
set cindent
set tabstop=8
set expandtab
set noerrorbells
set backup
set ruler
set showmode
highlight ModeMsg guibg=blue guifg=green gui=NONE cterm=NONE term=NONE
set wildignore=*.o,*.obj,*.class,*~,.bak,*.pyc,*.pyo,_build,dist,cabal-dev

set foldmethod=indent
set foldlevel=1
set foldcolumn=1

let g:tagbar_width=28

colorscheme two2tango

set autoread
set autowrite
set nohlsearch
set incsearch
set showmatch
set ignorecase

set spell spelllang=en

let g:detectindent_preferred_expandtab = 1
let g:detectindent_preferred_indent = 8

let &guicursor = &guicursor . ",a:blinkon0"

set encoding=utf-8

let g:SuperTabDefaultCompletionType = "context"
set completeopt=menuone,longest,preview

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

nnoremap <m-Down> :cnext<cr>zvzz
nnoremap <m-Up> :cprevious<cr>zvzz

let g:ctrlp_map = '<c-t>'

" haskell stuff
let g:haskell_conceal_wide = 1
let g:neocomplcache_enable_at_startup = 1

let g:haddock_browser="xdg-open"
let g:haddock_indexfiledir="/home/nicolas/.vim/"

let g:syntastic_haskell_checker="ghc-mod"

au Bufenter *.hs,*.lhs compiler ghc
autocmd BufWritePost *.hs GhcModCheckAndLintAsync

au FileType haskell let b:ghc_staticoptions = '-isrc -ibin'
let g:ghcmod_ghc_options = ['-isrc', '-ibin']
let g:ghcmod_use_basedir = getcwd()

au FileType haskell nnoremap <leader>t :GhcModType<cr>
au FileType haskell nnoremap <leader>T :GhcModTypeInsert<cr>

let g:necoghc_enable_detailed_browse = 1

