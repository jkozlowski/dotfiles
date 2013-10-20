set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Manage bundles  
Bundle 'gmarik/vundle'
Bundle 'zhaocai/GoldenView.Vim'
Bundle 'kien/ctrlp.vim'
Bundle 'eagletmt/tinytest'
Bundle 'mattn/gist-vim'
Bundle 'gregsexton/gitv'
Bundle 'sjl/gundo.vim'
Bundle 'vim-scripts/hgrev'
Bundle 'bitc/lushtags'
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
Bundle 'a.vim'
Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Bundle 'git://git.wincent.com/command-t.git'
Bundle 'jistr/vim-nerdtree-tabs'

" Haskell bundles
Bundle 'dag/vim2hs'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'Shougo/neocomplcache.vim'
Bundle 'scrooloose/syntastic'
Bundle 'bitc/vim-hdevtools'

filetype plugin indent on     " required!
