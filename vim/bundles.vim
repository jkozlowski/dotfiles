set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Manage bundles  
Bundle 'gmarik/vundle'
Bundle 'zhaocai/GoldenView.Vim'
Bundle 'kien/ctrlp.vim'
Bundle 'eagletmt/tinytest'
Bundle 'Shougo/vimproc.vim'
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
Bundle 'a.vim'
Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Bundle 'git://git.wincent.com/command-t.git'
Bundle 'nominolo/scion', {'rtp': 'vim_runtime_path'}
Bundle 'vim-scripts/hlint'
Bundle 'jistr/vim-nerdtree-tabs'

filetype plugin indent on     " required!
