" Manage Plugins using vim-plug
call plug#begin('~/.config/nvim/plugged')
Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'savq/melange'
Plug 'jalvesaq/vimcmdline'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'lervag/vimtex'
Plug 'dhruvasagar/vim-table-mode'
Plug 'python-mode/python-mode'
Plug 'alx741/vim-hindent'
Plug 'tpope/vim-surround'
Plug 'wellle/targets.vim'
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
Plug 'ervandew/supertab'
Plug 'tpope/vim-fugitive'
Plug 'tommcdo/vim-exchange'
Plug 'rhysd/vim-grammarous'
Plug 'ron89/thesaurus_query.vim'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
call plug#end()

"Configuration starts here
"General Settings
set number
filetype plugin on
filetype indent on
set ignorecase
set wildignorecase
set smartcase
set clipboard+=unnamedplus
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
let g:netrw_keepdir=0
if (has("termguicolors"))
 set termguicolors
endif
colorscheme melange

"customize statusline
"set noshowmode

" Keystrokes
let maplocalleader = "-"
let mapleader = "\<Space>"
map Y y$
nnoremap <leader>fs :w<cr>
nnoremap <leader>sw :write !sudo tee %<cr>
nnoremap <leader>w <C-w>

" Source/edit .vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>hdc :vsplit $MYVIMRC<cr>
vnoremap . :norm.<CR>
nnoremap <leader>n :nohlsearch<cr>
nnoremap <leader><leader> :
nnoremap <leader>o :!xdg-open 

" auto pairs
let g:AutoPairsFlyMode = 1

"nerdtree
nmap <leader>t :NERDTreeToggle<cr>

" vimcmdline mappings
let cmdline_map_start          = '<LocalLeader>s'
let cmdline_map_send           = '<LocalLeader>r'
let cmdline_map_send_and_stay  = '<LocalLeader><Space>'
let cmdline_map_source_fun     = '<LocalLeader>f'
let cmdline_map_send_paragraph = '<LocalLeader>p'
let cmdline_map_send_block     = '<LocalLeader>b'
let cmdline_map_quit           = '<LocalLeader>q'
let cmdline_app           = {}
let cmdline_app['sh']     = '/usr/bin/bash'

" vimcmdline options
let cmdline_vsplit      = 0      " Split the window vertically
let cmdline_esc_term    = 1      " Remap <Esc> to :stopinsert in Neovim's terminal
let cmdline_in_buffer   = 1      " Start the interpreter in a Neovim's terminal
let cmdline_term_height = 15     " Initial height of interpreter window or pane
let cmdline_term_width  = 80     " Initial width of interpreter window or pane
let cmdline_tmp_dir     = '/tmp' " Temporary directory to save files
let cmdline_outhl       = 1      " Syntax highlight the output
let cmdline_auto_scroll = 1      " Keep the cursor at the end of terminal (nvim)
autocmd BufEnter ~/.nvimshell.sh normal -s

" ctrlP 
let g:ctrlp_map = '<leader>p'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_user_command = 'fd --full-path --hidden -t f' 

" For Python 
let python_highlight_all=1
au BufNewFile,BufRead *.py set fileformat=unix
au BufNewFile,BufRead *.py let g:acp_enableAtStartup = 1
let g:pymode_python = 'python3'
let g:pymode_folding=0
let g:pymode_rope=0

" Latex
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_view_automatic=1
let g:vimtex_imaps_leader="#"
let g:vimtex_complete_close_braces = 1
let g:vimtex_quickfix_mode=0
let g:tex_flavor = 'latex'
au Filetype tex setlocal spell spelllang=en
"view header file
au Filetype tex nnoremap <buffer> <leader>vh :sp header.tex<cr>
"view main file
au Filetype tex nnoremap <buffer> <leader>vm :sp ../main.tex<cr>
"view bib file
au Filetype tex nnoremap <buffer> <leader>vb :sp bibliography.bib<cr>
" make link to main.tex
au Filetype tex nnoremap <buffer> <leader>ml ggi%! TEX root = ../main.tex <esc> o <esc>

"open frequently used directories
nnoremap <leader>fh :e ~/
nnoremap <leader>fd :e ~/Documents/
nnoremap <leader>fc :e ~/.config/

" Insert mode completion
set complete+=k
set dictionary+=/usr/share/dict/cracklib-small

" vim-sneak
let g:sneak#streak = 0
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F

" supertab
let g:SuperTabDefaultCompletionType = "<c-n>"

" vim-grammarous
nnoremap <leader>v <Plug>(grammarous-open-info-window)

set runtimepath+=,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.config/nvim

" gui settings
if exists("g:neovide")
    set guifont=Fira\ Code:h10
    let g:neovide_cursor_animation_length=0.0
endif
