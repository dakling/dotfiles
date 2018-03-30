" Manage Plugins using vim-plug
call plug#begin('~/.config/nvim/plugged')
Plug 'cocopon/iceberg.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'bkad/CamelCaseMotion'
"Plug 'jvirtanen/vim-octave'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'lervag/vimtex'
"Plug 'vim-scripts/gmsh.vim'
Plug 'dhruvasagar/vim-table-mode'
Plug 'jceb/vim-orgmode'
"Plug 'jalvesaq/Nvim-R'
"Plug 'nvie/vim-flake8'
Plug 'python-mode/python-mode'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'tpope/vim-surround'
Plug 'wellle/targets.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'bfredl/nvim-miniyank'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'justinmk/vim-sneak'
" Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
" Plug 'effi/vim-OpenFoam-syntax'
Plug 'ervandew/supertab'
Plug 'OmniSharp/omnisharp-vim'
Plug 'fsharp/vim-fsharp', {
      \ 'for': 'fsharp',
      \ 'do':  'make fsautocomplete',
      \}
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'l04m33/vlime'

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
colorscheme iceberg
"customize statusline
function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline guibg=Black ctermfg=110 guifg=LightSkyBlue3 ctermbg=0
  elseif a:mode == 'r'
    hi statusline guibg=Black ctermfg=140 guifg=Purple ctermbg=0
  else
    hi statusline guibg=Black ctermfg=1 guifg=DarkBlue ctermbg=0
  endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline guibg=LightSkyBlue3 ctermfg=0 guifg=Grey19 ctermbg=110
" default the statusline to grey when entering Vim
hi statusline guibg=LightSkyBlue3 ctermfg=0 guifg=Grey19 ctermbg=110

" Formats the statusline
set statusline=%f                           " file name
set statusline+=%y      "filetype
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=        " Switch to the right side
set statusline+=%c        " Current column
set statusline+=;         " Separator
set statusline+=%l        " Current line
set statusline+=/         " Separator
set statusline+=%L        " Total lines
" Keystrokes
let maplocalleader = "-"
let mapleader = "\<Space>"
map Y y$
"inoremap jk <ESC>
nnoremap <leader>fs :w<cr>
nnoremap <leader>sw :write !sudo tee %<cr>
nnoremap <leader>w <C-w>
" Source/edit .vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
vnoremap . :norm.<CR>
nnoremap <leader><leader> :
"nerdtree
nmap <leader>t :NERDTreeToggle<cr>
" camelCase
call camelcasemotion#CreateMotionMappings('ä')
" for openfoam
" change bc to zerogradient
" nnoremap <leader>fzg di{i<tab>type<tab>zerogradient;<cr><tab><esc>
" change bc to fixedvalue
" nnoremap <leader>ffv di{i<tab>type<tab>fixedvalue;<cr><tab><tab>value<tab>uniform <++>;<cr><tab><esc>
" for BoSSSPad
augroup filetypedetect
    autocmd bufnewfile,bufread *.bws set filetype=cs
augroup end 
" csharp
let g:OmniSharp_typeLookupInPreview = 1
" let g:OmniSharp_server_type = 'roslyn'
" Get Code Issues and syntax errors
let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']
" If you are using the omnisharp-roslyn backend, use the following
" let g:syntastic_cs_checkers = ['code_checker']
"Move the preview window (code documentation) to the bottom of the screen, so it doesn't move the code!
"You might also want to look at the echodoc plugin
set splitbelow
augroup omnisharp_commands
    autocmd!
    "Set autocomplete function to OmniSharp (if not using YouCompleteMe completion plugin)
    autocmd FileType cs setlocal omnifunc=OmniSharp#Complete
    " Synchronous build (blocks Vim)
    autocmd FileType cs nnoremap <F5> :wa!<cr>:OmniSharpBuild<cr>
    " Builds can also run asynchronously with vim-dispatch installed
    autocmd FileType cs nnoremap <localleader>b :wa!<cr>:OmniSharpBuildAsync<cr>
    " automatic syntax check on events (TextChanged requires Vim 7.4)
    autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck
    " Automatically add new cs files to the nearest project on save
    " autocmd BufWritePost *.cs call OmniSharp#AddToProject()
    "show type information automatically when the cursor stops moving
    autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()
    "The following commands are contextual, based on the current cursor position.
    autocmd FileType cs nnoremap gd :OmniSharpGotoDefinition<cr>
    autocmd FileType cs nnoremap <localleader>fi :OmniSharpFindImplementations<cr>
    autocmd FileType cs nnoremap <localleader>ft :OmniSharpFindType<cr>
    autocmd FileType cs nnoremap <localleader>fs :OmniSharpFindSymbol<cr>
    autocmd FileType cs nnoremap <localleader>fu :OmniSharpFindUsages<cr>
    "finds members in the current buffer
    autocmd FileType cs nnoremap <localleader>fm :OmniSharpFindMembers<cr>
    " cursor can be anywhere on the line containing an issue
    autocmd FileType cs nnoremap <localleader>x  :OmniSharpFixIssue<cr>
    autocmd FileType cs nnoremap <localleader>fx :OmniSharpFixUsings<cr>
    autocmd FileType cs nnoremap <localleader>tt :OmniSharpTypeLookup<cr>
    autocmd FileType cs nnoremap <localleader>dc :OmniSharpDocumentation<cr>
    "navigate up by method/property/field
    autocmd FileType cs nnoremap <C-K> :OmniSharpNavigateUp<cr>
    "navigate down by method/property/field
    autocmd FileType cs nnoremap <C-J> :OmniSharpNavigateDown<cr>
augroup END
" this setting controls how long to wait (in ms) before fetching type / symbol information.
set updatetime=2000
" Remove 'Press Enter to continue' message when type information is longer than one line.
set cmdheight=2

" Contextual code actions (requires CtrlP or unite.vim)
nnoremap <localleader><space> :OmniSharpGetCodeActions<cr>
" Run code actions with text selected in visual mode to extract method
vnoremap <localleader><space> :call OmniSharp#GetCodeActions('visual')<cr>

" rename with dialog
nnoremap <localleader>nm :OmniSharpRename<cr>
nnoremap <F2> :OmniSharpRename<cr>
" rename without dialog - with cursor on the symbol to rename... ':Rename newname'
command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")

" Force OmniSharp to reload the solution. Useful when switching branches etc.
nnoremap <localleader>rl :OmniSharpReloadSolution<cr>
nnoremap <localleader>cf :OmniSharpCodeFormat<cr>
" Load the current .cs file to the nearest project
nnoremap <localleader>tp :OmniSharpAddToProject<cr>

" Start the omnisharp server for the current solution
nnoremap <localleader>ss :OmniSharpStartServer<cr>
nnoremap <localleader>sp :OmniSharpStopServer<cr>
" Add syntax highlighting for types and interfaces
nnoremap <localleader>th :OmniSharpHighlightTypes<cr>
"Don't ask to save when changing buffers (i.e. when jumping to a type definition)
" set hidden
" Enable snippet completion, requires completeopt-=preview
let g:OmniSharp_want_snippet=1

" for gmsh
augroup filetypedetect
autocmd bufnewfile,bufread *.geo     setf gmsh
augroup end 
" For Python 
let python_highlight_all=1
au BufNewFile,BufRead *.py set fileformat=unix
au BufNewFile,BufRead *.py let g:acp_enableAtStartup = 1
let g:pymode_python = 'python3'
let g:pymode_folding=0
let g:pymode_rope=0
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
" change cursor shape in insert mode
" let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
" Killring shortcuts
map p <Plug>(miniyank-autoput)
map P <Plug>(miniyank-autoPut)
map <leader>p <Plug>(miniyank-cycle)
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"open frequently used directories
nnoremap <leader>fh :e ~/
nnoremap <leader>fd :e ~/Dokumente/
nnoremap <leader>fc :e ~/.config/
" Insert mode completion
set complete+=k
set dictionary+=/usr/share/dict/cracklib-small
" vim-sneak
let g:sneak#streak = 0
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
" vim-easymotion
" <Leader>f{char} to move to {char}
" map <leader> <Plug>(easymotion-prefix)
" map  <leader>f <Plug>(easymotion-bd-f)
" nmap <leader>f <Plug>(easymotion-overwin-f)
" Move to word
" map  <leader>w <Plug>(easymotion-bd-w)
" nmap <leader>w <Plug>(easymotion-overwin-w)
" " Prevent repetitive use of hjkl
" nmap <leader>l <Plug>(easymotion-lineforward)
" nmap <leader>j <Plug>(easymotion-j)
" nmap <leader>k <Plug>(easymotion-k)
" nmap <leader>h <Plug>(easymotion-linebackward)
" anki-vim
autocmd BufRead *.anki_vim inoremap # ->
" Use deoplete.
let g:deoplete#enable_at_startup = 1
" call deoplete#custom#set('ulitsnips', 'matchers', ['matcher_fuzzy'])
" complete with tab
inoremap <silent><expr> <TAB>
\ pumvisible() ? "\<C-n>" :
\ <SID>check_back_space() ? "\<TAB>" :
\ deoplete#mappings#manual_complete()
function! s:check_back_space() abort "{{{
let col = col('.') - 1
return !col || getline('.')[col - 1]  =~ '\s'
endfunction"}}}
" supertab
let g:SuperTabDefaultCompletionType = "<c-n>"
" let g:SuperTabClosePreviewOnPopupClose = 1
"set runtimepath
set runtimepath+=,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.config/nvim
