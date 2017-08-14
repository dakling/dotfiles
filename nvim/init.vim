" Manage Plugins using vim-plug
call plug#begin('~/.config/nvim/plugged')
"Plug 'morhetz/gruvbox'
Plug 'cocopon/iceberg.vim'
Plug 'jiangmiao/auto-pairs'
"Plug 'jvirtanen/vim-octave'
Plug 'scrooloose/nerdcommenter'
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/syntastic'
Plug 'lervag/vimtex'
"Plug 'vim-scripts/gmsh.vim'
Plug 'dhruvasagar/vim-table-mode'
Plug 'jceb/vim-orgmode'
"Plug 'jalvesaq/Nvim-R'
"Plug 'nvie/vim-flake8'
Plug 'python-mode/python-mode'
Plug 'tpope/vim-surround'
Plug 'ajh17/VimCompletesMe'
Plug 'bfredl/nvim-miniyank'
"Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
"Plug 'junegunn/fzf.vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'justinmk/vim-sneak'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
"Plug 'effi/vim-OpenFoam-syntax'
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
set expandtab
let g:netrw_keepdir=0
set background=dark
"let g:gruvbox_contrast_dark="soft"
"let g:gruvbox_contrast_light="hard"
colorscheme iceberg
"customize statusline
function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline guibg=cyan ctermfg=110 guifg=Black ctermbg=0
  elseif a:mode == 'r'
    hi statusline guibg=Purple ctermfg=140 guifg=Black ctermbg=0
  else
    hi statusline guibg=DarkBlue ctermfg=1 guifg=Black ctermbg=0
  endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline guibg=DarkGrey ctermfg=8 guifg=White ctermbg=110

" default the statusline to grey when entering Vim
hi statusline guibg=DarkGrey ctermfg=8 guifg=White ctermbg=110

" Formats the statusline
set statusline=%f                           " file name
set statusline+=%y      "filetype
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=        " Switch to the right side
set statusline+=%l        " Current line
set statusline+=/         " Separator
set statusline+=%L        " Total lines
" Keystrokes
let maplocalleader = "-"
let mapleader = "\<Space>"
"inoremap jk <ESC>
nnoremap <leader>w :w<cr>
nnoremap <leader>q :x<cr>
nnoremap <leader>sw :write !sudo tee %<cr>
nnoremap Y y$
" Source/edit .vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
vnoremap . :norm.<CR>
nnoremap <leader><leader> :
" for openfoam
" change bc to zerogradient
nnoremap <leader>fzg di{i<tab>type<tab>zerogradient;<cr><tab><esc>
" change bc to fixedvalue
nnoremap <leader>ffv di{i<tab>type<tab>fixedvalue;<cr><tab><tab>value<tab>uniform <++>;<cr><tab><esc>
" for gmsh
augroup filetypedetect
autocmd bufnewfile,bufread *.geo     setf gmsh
augroup end 
" For Python 
let python_highlight_all=1
au BufNewFile,BufRead *.py set fileformat=unix
au BufNewFile,BufRead *.py let g:acp_enableAtStartup = 1
let g:pymode_python = 'python3'
" UltiSnips config
let g:UltiSnipsExpandTrigger="xx"
let g:UltiSnipsJumpForwardTrigger="xx"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:UltiSnipsUsePythonVersion = 3
let g:UltiSnipsEditSplit = "vertical"
let g:UltiSnipsSnippetsDir = $HOME."/.config/nvim/my-snippets"
let g:UltiSnipsSnippetDirectories = ['UltiSnips', $HOME.'/.config/nvim/my-snippets']
let g:UltiSnipsEnableSnipMate = 0
" Latex
let g:vimtex_view_automatic=1
let g:vimtex_imaps_leader="#"
let g:vimtex_view_method='zathura'
let g:vimtex_complete_close_braces = 1
let g:tex_flavor = 'latex'
au Filetype tex setlocal spell spelllang=en
"au Filetype tex setlocal textwidth=80
"view header file
au Filetype tex nnoremap <buffer> <leader>vh :sp header.tex<cr>
"view main file
au Filetype tex nnoremap <buffer> <leader>vm :sp ../main.tex<cr>
"view bib file
au Filetype tex nnoremap <buffer> <leader>vb :sp ../bibliography.bib<cr>
" make link to main.tex
au Filetype tex nnoremap <buffer> <leader>ml ggi%! TEX root = ../main.tex <esc> o <esc>

augroup VimCompletesMeTex
    autocmd!
    autocmd FileType tex let b:vcm_omni_pattern = 
        \ '\v\\%('
        \ . '\a*cite\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|\a*ref%(\s*\{[^}]*|range\s*\{[^,}]*%(}\{)?)'
        \ . '|hyperref\s*\[[^]]*'
        \ . '|includegraphics\*?%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|%(include%(only)?|input)\s*\{[^}]*'
        \ . '|\a*(gls|Gls|GLS)(pl)?\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|includepdf%(\s*\[[^]]*\])?\s*\{[^}]*'
        \ . '|includestandalone%(\s*\[[^]]*\])?\s*\{[^}]*'
        \ . '|usepackage%(\s*\[[^]]*\])?\s*\{[^}]*'
        \ . '|documentclass%(\s*\[[^]]*\])?\s*\{[^}]*'
        \ . ')'

augroup END

" change cursor shape in insert mode
let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
" Killring shortcuts
map p <Plug>(miniyank-autoput)
map P <Plug>(miniyank-autoPut)
map <leader>p <Plug>(miniyank-cycle)
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"fzf
nnoremap <leader>o :FZF<CR>
nnoremap <leader>h :FZF ~<CR>
"open frequently used directories
nnoremap <leader>fh :e ~/
nnoremap <leader>fd :e ~/Dokumente/
nnoremap <leader>fc :e ~/.config/

" Mapping selecting mappings
nnoremap <leader><tab> <plug>(fzf-maps-n)
xnoremap <leader><tab> <plug>(fzf-maps-x)
onoremap <leader><tab> <plug>(fzf-maps-o)
" Insert mode completion
inoremap <c-x><c-k> <plug>(fzf-complete-word)
inoremap <c-x><c-l> <plug>(fzf-complete-line)
" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})
" Replace the default dictionary completion with fzf-based fuzzy completion
inoremap <expr> <c-x><c-k> fzf#complete('cat /usr/share/dict/american-english')
"inoremap <expr> <c-x><c-k> fzf#complete('cat /usr/share/dict/german')
" vim-sneak
let g:sneak#streak = 0
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
" vim-easymotion
" <Leader>f{char} to move to {char}
map ö <Plug>(easymotion-prefix)
map  öf <Plug>(easymotion-bd-f)
nmap öf <Plug>(easymotion-overwin-f)
" Move to word
map  öw <Plug>(easymotion-bd-w)
nmap öw <Plug>(easymotion-overwin-w)
" Prevent repetitive use of hjkl
nmap öl <Plug>(easymotion-lineforward)
nmap öj <Plug>(easymotion-j)
nmap ök <Plug>(easymotion-k)
nmap öh <Plug>(easymotion-linebackward)
" anki-vim
autocmd BufRead *.anki_vim inoremap # ->
"set runtimepath
set runtimepath+=,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.config/nvim
