" Manage Plugins using vim-plug
call plug#begin('~/.config/nvim/plugged')
Plug 'cocopon/iceberg.vim'
" Plug 'jiangmiao/auto-pairs'
Plug 'Raimondi/delimitMate'
Plug 'bkad/CamelCaseMotion' 
"Plug 'jvirtanen/vim-octave'
Plug 'tpope/vim-commentary'
Plug 'vim-syntastic/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'lervag/vimtex'
Plug 'ludovicchabant/vim-gutentags'
"Plug 'vim-scripts/gmsh.vim'
Plug 'dhruvasagar/vim-table-mode'
Plug 'jceb/vim-orgmode'
"Plug 'jalvesaq/Nvim-R'
"Plug 'nvie/vim-flake8'
Plug 'python-mode/python-mode'
Plug 'neovimhaskell/haskell-vim'
Plug 'alx741/vim-hindent'
Plug 'w0rp/ale'
" Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'parsonsmatt/intero-neovim'
" Plug 'eagletmt/neco-ghc'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'tpope/vim-surround'
Plug 'wellle/targets.vim'
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Valloric/YouCompleteMe'
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
" Plug 'OmniSharp/omnisharp-vim'
" Plug 'fsharp/vim-fsharp', {
"       \ 'for': 'fsharp',
"       \ 'do':  'make fsautocomplete',
"       \}
" Plug 'tpope/vim-dispatch'
" Plug 'radenling/vim-dispatch-neovim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'l04m33/vlime'
Plug 'tommcdo/vim-exchange'
" Plug 'vimoutliner/vimoutliner'
Plug 'mattn/calendar-vim'
" Plug 'itchyny/calendar.vim' google sync but no vimwiki compatibily
Plug 'vimwiki/vimwiki', { 'branch': 'dev' }
Plug 'rhysd/vim-grammarous'
" Plug 'reedes/vim-wordy'
Plug 'ron89/thesaurus_query.vim'
" Plug 'dbmrq/vim-ditto'
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
" set statusline+=%#warningmsg# " syntastic
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
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
nnoremap <leader>e <C-w>
" Source/edit .vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
vnoremap . :norm.<CR>
nnoremap <leader><leader> :
" auto pairs
let g:AutoPairsFlyMode = 1
"nerdtree
nmap <leader>t :NERDTreeToggle<cr>
" camelCase
call camelcasemotion#CreateMotionMappings('#')
" for openfoam
" change bc to zerogradient
" nnoremap <leader>fzg di{i<tab>type<tab>zerogradient;<cr><tab><esc>
" change bc to fixedvalue
" nnoremap <leader>ffv di{i<tab>type<tab>fixedvalue;<cr><tab><tab>value<tab>uniform <++>;<cr><tab><esc>
" for BoSSSPad
augroup filetypedetect
    autocmd bufnewfile,bufread *.bws set filetype=cs
augroup end 
" csharp using YouCompleteMe + OmniSharp
augroup omnisharp_commands
    autocmd!
    " Synchronous build (blocks Vim)
    " autocmd FileType cs nnoremap <buffer> <F5> :wa!<CR>:OmniSharpBuild<CR>
    " Builds can also run asynchronously with vim-dispatch installed
    " autocmd FileType cs nnoremap <buffer> <Leader>b :wa!<CR>:OmniSharpBuildAsync<CR>
    " Automatic syntax check on events (TextChanged requires Vim 7.4)
    autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck

    " Automatically add new cs files to the nearest project on save
    " autocmd BufWritePost *.cs call OmniSharp#AddToProject()

    " Show type information automatically when the cursor stops moving
    " autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

    " The following commands are contextual, based on the cursor position.
    autocmd FileType cs nnoremap <buffer> gd :YcmCompleter GoToDeclaration<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>gd :YcmCompleter GoToDefinition<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>gi :YcmCompleter GoToImplementation<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>gt :YcmCompleter GetType<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>gc :YcmCompleter GetDoc<CR>
    " Cursor can be anywhere on the line containing an issue
    autocmd FileType cs nnoremap <buffer> <Leader>x :YcmCompleter FixIt<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>r :YcmCompleter RefactorRename 
    " autocmd FileType cs nnoremap <buffer> <Leader>fx :OmniSharpFixUsings<CR>
    " autocmd FileType cs nnoremap <buffer> <Leader>tt :OmniSharpTypeLookup<CR>
    " autocmd FileType cs nnoremap <buffer> <Leader>dc :OmniSharpDocumentation<CR>

    " Navigate up and down by method/property/field
    " autocmd FileType cs nnoremap <buffer> <C-k> :OmniSharpNavigateUp<CR>
    " autocmd FileType cs nnoremap <buffer> <C-j> :OmniSharpNavigateDown<CR>
augroup END

" csharp
"" " Set the path to the roslyn server
"" legacy server
"let g:OmniSharp_server_path = '/home/klingenberg/Documents/programming/c#/omnisharp-roslyn/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe'
"" new server
"" let g:OmniSharp_server_path = '/home/klingenberg/Documents/programming/c#/omnisharp-roslyn/roslyn/omnisharp/OmniSharp.exe'
"" let g:OmniSharp_server_path = '/opt/omnisharp-roslyn/OmniSharp.exe'

"" Set the type lookup function to use the preview window instead of echoing it
"let g:OmniSharp_typeLookupInPreview = 1

"" Timeout in seconds to wait for a response from the server
"let g:OmniSharp_timeout = 100

"" Don't autoselect first omnicomplete option, show options even if there is only
"" one (so the preview documentation is accessible). Remove 'preview' if you
"" don't want to see any documentation whatsoever.
"set completeopt=longest,menuone,preview

"" Fetch full documentation during omnicomplete requests.
"" There is a performance penalty with this (especially on Mono).
"" By default, only Type/Method signatures are fetched. Full documentation can
"" still be fetched when you need it with the :OmniSharpDocumentation command.
""let g:omnicomplete_fetch_full_documentation = 1

"" Set desired preview window height for viewing documentation.
"" You might also want to look at the echodoc plugin.
"set previewheight=5

"" let g:OmniSharp_server_type = 'roslyn'
"" use legacy server
"let g:OmniSharp_server_type = 'v1'
"" use legacy server and get code issues and syntax errors
"let g:syntastic_cs_checkers = ['code_checker']

"" let g:OmniSharp_selector_ui = ''       " Use vim - command line, quickfix etc.

"let g:OmniSharp_server_use_mono = 1

"augroup omnisharp_commands
"    autocmd!
"    " Synchronous build (blocks Vim)
"    autocmd FileType cs nnoremap <buffer> <F5> :wa!<CR>:OmniSharpBuild<CR>
"    " Builds can also run asynchronously with vim-dispatch installed
"    autocmd FileType cs nnoremap <buffer> <Leader>b :wa!<CR>:OmniSharpBuildAsync<CR>
"    " Automatic syntax check on events (TextChanged requires Vim 7.4)
"    autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck

"    " Automatically add new cs files to the nearest project on save
"    autocmd BufWritePost *.cs call OmniSharp#AddToProject()

"    " Show type information automatically when the cursor stops moving
"    autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

"    " The following commands are contextual, based on the cursor position.
"    autocmd FileType cs nnoremap <buffer> gd :OmniSharpGotoDefinition<CR>
"    autocmd FileType cs nnoremap <buffer> <Leader>fi :OmniSharpFindImplementations<CR>
"    autocmd FileType cs nnoremap <buffer> <Leader>fb :OmniSharpFindSymbol<CR>
"    autocmd FileType cs nnoremap <buffer> <Leader>fu :OmniSharpFindUsages<CR>

"    " Finds members in the current buffer
"    autocmd FileType cs nnoremap <buffer> <Leader>fm :OmniSharpFindMembers<CR>

"    " Cursor can be anywhere on the line containing an issue
"    autocmd FileType cs nnoremap <buffer> <Leader>x  :OmniSharpFixIssue<CR>
"    autocmd FileType cs nnoremap <buffer> <Leader>fx :OmniSharpFixUsings<CR>
"    autocmd FileType cs nnoremap <buffer> <Leader>tt :OmniSharpTypeLookup<CR>
"    autocmd FileType cs nnoremap <buffer> <Leader>dc :OmniSharpDocumentation<CR>

"    " Navigate up and down by method/property/field
"    autocmd FileType cs nnoremap <buffer> <C-k> :OmniSharpNavigateUp<CR>
"    autocmd FileType cs nnoremap <buffer> <C-j> :OmniSharpNavigateDown<CR>
"augroup END

"" Contextual code actions (uses fzf, CtrlP or unite.vim when available)
"nnoremap <Leader>gca :OmniSharpGetCodeActions<CR>
"" Run code actions with text selected in visual mode to extract method
"xnoremap <Leader><Space> :call OmniSharp#GetCodeActions('visual')<CR>

"" Rename with dialog
"nnoremap <Leader>nm :OmniSharpRename<CR>
"nnoremap <F2> :OmniSharpRename<CR>
"" Rename without dialog - with cursor on the symbol to rename: `:Rename newname`
"command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")

"" Force OmniSharp to reload the solution. Useful when switching branches etc.
"nnoremap <Leader>rl :OmniSharpReloadSolution<CR>
"nnoremap <Leader>cf :OmniSharpCodeFormat<CR>
"" Load the current .cs file to the nearest project
"nnoremap <Leader>tp :OmniSharpAddToProject<CR>

"" Start the omnisharp server for the current solution
"nnoremap <Leader>ss :OmniSharpStartServer<CR>
"nnoremap <Leader>sp :OmniSharpStopServer<CR>

"" Add syntax highlighting for types and interfaces
"nnoremap <Leader>th :OmniSharpHighlightTypes<CR>

"" Enable snippet completion
"" let g:OmniSharp_want_snippet=1
" for gmsh
augroup filetypedetect
autocmd bufnewfile,bufread *.geo     setf gmsh
augroup end 
" Haskell
" ----- neovimhaskell/haskell-vim -----
" Align 'then' two spaces after 'if'
let g:haskell_indent_if = 2
" Indent 'where' block two spaces under previous body
let g:haskell_indent_before_where = 2
" Allow a second case indent style (see haskell-vim README)
let g:haskell_indent_case_alternative = 1
" Only next under 'let' if there's an equals sign
let g:haskell_indent_let_no_in = 0
" ----- hindent & stylish-haskell -----
" Indenting on save is too aggressive for me
let g:hindent_on_save = 0
" Helper function, called below with mappings
function! HaskellFormat(which) abort
  if a:which ==# 'hindent' || a:which ==# 'both'
    :Hindent
  endif
  if a:which ==# 'stylish' || a:which ==# 'both'
    silent! exe 'undojoin'
    silent! exe 'keepjumps %!stylish-haskell'
  endif
endfunction
" Key bindings
augroup haskellStylish
  au!
  " Just hindent
  au FileType haskell nnoremap <localleader>hi :Hindent<CR>
  " Just stylish-haskell
  au FileType haskell nnoremap <localleader>hs :call HaskellFormat('stylish')<CR>
  " First hindent, then stylish-haskell
  au FileType haskell nnoremap <localleader>hf :call HaskellFormat('both')<CR>
augroup END
" ----- w0rp/ale -----
" let g:ale_linters.haskell = ['hlint']
" ----- parsonsmatt/intero-neovim -----
" Prefer starting Intero manually (faster startup times)
" let g:intero_start_immediately = 0
" " Use ALE (works even when not using Intero)
let g:intero_use_neomake = 0
augroup interoMaps
  au!
  au FileType haskell nnoremap <silent> <localleader>io :InteroOpen<CR>
  au FileType haskell nnoremap <silent> <localleader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <localleader>ih :InteroHide<CR>
  au FileType haskell nnoremap <silent> <localleader>is :InteroStart<CR>
  au FileType haskell nnoremap <silent> <localleader>ik :InteroKill<CR>

  au FileType haskell nnoremap <silent> <localleader>wr :w \| :InteroReload<CR>
  au FileType haskell nnoremap <silent> <localleader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <localleader>if :InteroLoadCurrentFile<CR>

  au FileType haskell map <localleader>t <Plug>InteroGenericType
  au FileType haskell map <localleader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <localleader>it :InteroTypeInsert<CR>

  au FileType haskell nnoremap <silent> <localleader>jd :InteroGoToDef<CR>
  au FileType haskell nnoremap <silent> <localleader>iu :InteroUses<CR>
  au FileType haskell nnoremap <localleader>ist :InteroSetTargets<SPACE>
  au FileType haskell nnoremap <localleader>b :InteroEval main<CR>
  au FileType haskell nnoremap <localleader>ie :InteroEval 
augroup END
augroup filetypedetect
    autocmd bufnewfile,bufread *.hs nnoremap <Leader>r :! runhaskell % <CR>
augroup end 
" autocompletion
" let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
let g:ycm_semantic_triggers = {'haskell' : ['.']}
" For Python 
let python_highlight_all=1
au BufNewFile,BufRead *.py set fileformat=unix
au BufNewFile,BufRead *.py let g:acp_enableAtStartup = 1
let g:pymode_python = 'python3'
let g:pymode_folding=0
let g:pymode_rope=0
let g:python_host_prog = '/usr/bin/python2'
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
let g:vimtex_compiler_progname = 'nvr'
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
au Filetype tex nnoremap <buffer> <leader>vb :sp bibliography.bib<cr>
"view local config
au Filetype tex nnoremap <buffer> <leader>vl :sp definLocal.tex<cr>
"view global config
au Filetype tex nnoremap <buffer> <leader>vg :sp $LatexGlobalConfig<cr>
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
" syntastic
let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_quiet_messages = { "regex": [
        \ '\mpossible unwanted space at "{"',
        \ ] }
let g:syntastic_ignore_files = [
        \ '\mdefinLocal.tex',
        \ ]
" vimwiki/vimwiki
let g:vimwiki_list = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
" Use autocmds to check your text automatically and keep the highlighting
" up to date (easier):
" au FileType markdown,text,tex DittoOn  " Turn on Ditto's autocmds
" nmap <leader>di <Plug>ToggleDitto      " Turn Ditto on and off

" If you don't want the autocmds, you can also use an operator to check
" specific parts of your text:
" vmap <leader>d <Plug>Ditto	       " Call Ditto on visual selection
" nmap <leader>d <Plug>Ditto	       " Call Ditto on operator movement

" nmap =d <Plug>DittoNext                " Jump to the next word
" nmap -d <Plug>DittoPrev                " Jump to the previous word
" nmap +d <Plug>DittoGood                " Ignore the word under the cursor
" nmap _d <Plug>DittoBad                 " Stop ignoring the word under the cursor
" nmap ]d <Plug>DittoMore                " Show the next matches
" nmap [d <Plug>DittoLess                " Show the previous matches
" vim-grammarous
nnoremap <leader>v <Plug>(grammarous-open-info-window)
"set runtimepath
set runtimepath+=,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.config/nvim
