" Manage Plugins using vim-plug
call plug#begin('~/.config/nvim/plugged')
Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'mhartington/oceanic-next'
Plug 'jalvesaq/vimcmdline'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
call plug#end()
"Configuration starts here
"General Settings
"set textwidth=100 
set number
filetype plugin on
filetype indent on
set ignorecase
set wildignorecase
set smartcase
set autochdir
set clipboard+=unnamedplus
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
set autoread
let g:netrw_keepdir=0
" set background=dark
if (has("termguicolors"))
 set termguicolors
endif
let g:oceanic_next_terminal_bold = 1
let g:oceanic_next_terminal_italic = 1
colorscheme OceanicNext
" let g:onedark_terminal_italics=1
"customize statusline
set noshowmode
let g:lightline= {
            \'colorscheme': 'oceanicnext',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ],
            \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
            \ },
            \ 'component_function': {
            \   'gitbranch': 'fugitive#head'
            \ },
            \}
" Keystrokes
let maplocalleader = "-"
let mapleader = "\<Space>"
map Y y$
"inoremap jk <ESC>
nnoremap <leader>fs :w<cr>
nnoremap <leader>w <C-w>
nnoremap <leader><leader> :
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
let cmdline_app['sh']     = '/usr/bin/zsh'
" vimcmdline options
let cmdline_vsplit      = 0      " Split the window vertically
let cmdline_esc_term    = 1      " Remap <Esc> to :stopinsert in Neovim's terminal
let cmdline_in_buffer   = 1      " Start the interpreter in a Neovim's terminal
let cmdline_term_height = 30     " Initial height of interpreter window or pane
let cmdline_term_width  = 80     " Initial width of interpreter window or pane
let cmdline_tmp_dir     = '/tmp' " Temporary directory to save files
let cmdline_outhl       = 1      " Syntax highlight the output
let cmdline_auto_scroll = 1      " Keep the cursor at the end of terminal (nvim)
autocmd BufEnter ~/.nvimshell/nvimshell.sh normal -s
autocmd BufEnter ~/.nvimshell/nvimshell.sh silent ! ls /usr/bin > ~/.binlist
" disable warning
augroup SwapClobber 
au! 
au SwapExists * let v:swapchoice='d' 
augroup END 
" Insert mode completion
set complete+=k
set dictionary+=~/.binlist
set runtimepath+=,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.config/nvim
"abbrevations
iabbrev nt nvim-termite
