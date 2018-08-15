" Plugins -------------------------------------------------------------------{{{

  if (!isdirectory(expand("$HOME/.config/nvim/repos/github.com/Shougo/dein.vim")))
    call system(expand("mkdir -p $HOME/.config/nvim/repos/github.com"))
    call system(expand("git clone https://github.com/Shougo/dein.vim $HOME/.config/nvim/repos/github.com/Shougo/dein.vim"))
  endif

  set runtimepath+=~/.config/nvim/repos/github.com/Shougo/dein.vim/
  call dein#begin(expand('~/.config/nvim'))

  call dein#add('Shougo/dein.vim')
  call dein#add('haya14busa/dein-command.vim')
  call dein#add('scrooloose/nerdtree')
  call dein#add('junegunn/fzf', {'build': './install --all', 'merged': 0})
  call dein#add('junegunn/fzf.vim', {'depends': 'fzf'})
  call dein#add('tpope/vim-surround')
  call dein#add('terryma/vim-multiple-cursors')
  call dein#add('easymotion/vim-easymotion')
  call dein#add('matze/vim-move')
  call dein#add('tomtom/tcomment_vim')
  call dein#add('itmammoth/doorboy.vim')
  call dein#add('valloric/MatchTagAlways', {'on_ft': 'html'})
  call dein#add('embear/vim-localvimrc')
  call dein#add('dyng/ctrlsf.vim')

  call dein#add('Shougo/deoplete.nvim')
  call dein#add('Shougo/neco-vim')
  call dein#add('Shougo/neco-syntax')
  call dein#add('ternjs/tern_for_vim', {'build': 'npm install'})
  call dein#add('carlitux/deoplete-ternjs', {'build': 'npm install -g tern'})
  " call dein#add('othree/jspc.vim')
  " call dein#add('zchee/deoplete-jedi')

  call dein#add('elmcast/elm-vim')

  call dein#add('neoclide/vim-jsx-improve')
  " call dein#add('pangloss/vim-javascript')
  " call dein#add('mxw/vim-jsx')
  call dein#add('HerringtonDarkholme/yats.vim')
  call dein#add('Quramy/vim-js-pretty-template')
  call dein#add('mhartington/nvim-typescript')
  call dein#add('kchmck/vim-coffee-script')
  call dein#add('SirVer/ultisnips')
  call dein#add('mattn/emmet-vim')
  call dein#add('vimwiki/vimwiki')
  call dein#add('christoomey/vim-tmux-navigator')
  call dein#add('benmills/vimux')
  call dein#add('godlygeek/tabular')
  call dein#add('editorconfig/editorconfig-vim')
  call dein#add('w0rp/ale')

  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('mhartington/oceanic-next')
  call dein#add('chriskempson/base16-vim')
  call dein#add('arcticicestudio/nord-vim')
  call dein#add('ryanoasis/vim-devicons')
  call dein#add('yggdroot/indentline')
  call dein#add('mhinz/vim-startify')

  call dein#add('tpope/vim-fugitive')
  call dein#add('junegunn/gv.vim')
  call dein#add('airblade/vim-gitgutter')

  call dein#add('tmhedberg/SimpylFold.git')
  call dein#add('vim-scripts/indentpython.vim')

  call dein#add('pelodelfuego/vim-swoop.git')

  call dein#add('cespare/vim-toml')

  if dein#check_install()
    call dein#install()
    let pluginsExist=1
  endif

  call dein#end()
  filetype plugin indent on

  " }}}

" Settings ------------------------------------------------------------------{{{

  " language en_US
  let mapleader=','

  if (has("termguicolors"))
    set termguicolors
    set t_8f=^[[38;2;%lu;%lu;%lum
    set t_8b=^[[48;2;%lu;%lu;%lum
  endif
  " Display extra whitespace
  set list listchars=tab:»·,trail:·,nbsp:·
  set scrolloff=0
  " set clipboard+=unnamedplus
  set noshowmode
  set noswapfile
  set nobackup
  set nowritebackup
  set number
  set relativenumber
  set numberwidth=4
  set cursorline
  set tabstop=2 shiftwidth=2 expandtab
  set autoindent
  set shiftround
  set conceallevel=0
  set virtualedit=block
  set wildmenu
  set laststatus=2
  set lazyredraw
  set wrap linebreak
  set wildmode=full
  set autoread
  set complete=.,w,b,u,t,k
  set formatoptions+=t
  set inccommand=nosplit
  set shortmess=atIc
  set isfname-==
  set suffixesadd=.js,.coffee,.jsx

  " Clear trailing whitespace when saving buffer
  autocmd BufWritePre * %s/\s\+$//e
  " Turn off relative numbering when going into insert mode
  " autocmd InsertEnter,InsertLeave * set rnu!
  " Remember cursor position between vim sessions
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line ("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif
  " center buffer around cursor when opening files
  autocmd BufRead * normal zz
  " autocmd InsertEnter * let save_cwd=getcwd() | set autochdir
  " autocmd InsertLeave * set noautochdir | execute 'cd' fnameescape(save_cwd)

  au TermOpen * setlocal nonumber norelativenumber

" }}}

" Look & Feel ---------------------------------------------------------------{{{

  set background=dark
  syntax on
  " colorscheme base16-oceanicnext
  colorscheme nord

  " highlight Normal guibg=NONE
  highlight CursorLineNr guifg=White gui=NONE
  highlight Comment guifg=#5C667A
  highlight ALEErrorSign guifg=#ec5f67 guibg=#343d46
  highlight ALEErrorSign guifg=#ec5f67 guibg=#343d46

  let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols={}
  let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['js']=' '
  let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['vim']=''

  let g:startify_fortune_use_unicode=1
  let g:startify_change_to_vcs_root=1
  let g:startify_list_order = [
        \ ['    Recent Files:'], 'files',
        \ ['    Project:'], 'dir',
        \ ['    Bookmarks:'], 'bookmarks',
        \ ['    Sessions:'], 'sessions',
        \ ['    Commands:'], 'commands'
        \ ]
  let g:startify_custom_header =
        \ map(startify#fortune#boxed(), '"   ".v:val')

" }}}

" Key mappings  -------------------------------------------------------------{{{

  " Copy to OSX clipboard
  " vnoremap <leader>y "*y<CR>
  " nnoremap <leader>Y "*Y<CR>

  " nnoremap <silent><leader>p :set paste<CR>"*p:set nopaste<CR>
  " nnoremap <silent><leader>P :set paste<CR>"*P:set nopaste<CR>

  " nnoremap <leader>v :ed ~/.config/nvim/init.vim<CR><c-w>o<CR>:below new ~/.tmux.conf<CR>:vsp ~/.tmux/tmux.binds.conf<CR>
  nnoremap <leader>v :sp ~/.config/nvim/init.vim<CR>
  nnoremap <leader>u :source ~/.config/nvim/init.vim<CR>

  nnoremap <leader>jf :%!python -m json.tool<CR>

  " exit insert, dd line and re-enter insert
  " inoremap <c-k> <esc>ddi

  " movement
  inoremap <c-k> <up>
  inoremap <c-j> <down>
  inoremap <c-h> <left>
  inoremap <c-l> <right>

  " nnoremap j gj
  " nnoremap k gk

  " buffer operations
  nnoremap ﬂ :bnext<CR>
  nnoremap ˘ :bprevious<CR>
  nnoremap <leader>q :bdelete<CR>
  nnoremap <leader>bd :bdelete<CR>
  nnoremap <leader>bb :Buffers<CR>
  " nnoremap <leader>bo :call CloseAllBuffersButCurrent()<CR>
  " Switch between the last two buffers
  " nnoremap <leader>bb <c-^>
  nnoremap <leader><tab> <c-^>

  " window movement
  " nmap <c-j> <c-w>j
  " nmap <c-k> <c-w>k
  " nmap <c-l> <c-w>l
  " nmap <c-h> <c-w>h

  " Zoom windows
  nnoremap <leader>- :wincmd _<CR>:wincmd \|<CR>
  nnoremap <leader>= :wincmd =<CR>

  " vim-move (Alt + jk)
  vnoremap √ <Plug>MoveBlockDown
  vnoremap ª <Plug>MoveBlockUp
  nnoremap √ <Plug>MoveLineDown
  nnoremap ª <Plug>MoveLineUp

  " bind K to search for word under cursor
  nnoremap K :Find <C-R><C-W><CR>
  vnoremap K :Find <C-R><C-W><CR>

  nnoremap <c-s> :w<CR>
  nnoremap <leader>h :nohl<CR>
  nnoremap <leader>n :set rnu!<CR>

  nnoremap <leader>td :TernDef<CR>zz
  nnoremap <leader>tr :TernRename<CR>

  " autocmd FileType javascript nnoremap <leader>r :noautocmd vimgrep /\(function\)\? \?[^\( \|:\|(\)]\+:\? \?\(function\)\?([^(]*) \?{/j %<CR>:cw<CR>
  " autocmd FileType coffee nnoremap <leader>r :noautocmd vimgrep /^[^ ]\+ = \((.*)\)\? \?->/j %<CR>:cw<CR>
  " autocmd FileType qf nnoremap <buffer> <CR> <CR>:cclose<CR>zz

  nmap <leader>gs :Gstatus<CR>
  nmap <leader>gb :Gblame<CR>
  nmap <leader>gd :Gvdiff<CR>
  nmap <leader>ge :Gedit<CR>
  nmap <leader>gl :GV<CR>
  nmap <leader>gL :GV!<CR>
  nmap <leader>gp :Gpush<CR>
  nmap <leader>gpf :Gpush --force-with-lease<CR>

  nmap <c-p> :Files<CR>
  nmap <leader>f :Files<CR>
  nmap π :Files<CR>
  nmap <leader>F :call CustomFind()<CR>
  vmap ƒ <Plug>CtrlSFVwordPath<CR>
  nmap ƒ <Plug>CtrlSFCwordPath<CR>
  nmap ƒƒ :CtrlSFToggle<CR>
  nmap ∫ <Plug>CtrlSFPrompt
  nmap ∏ :Commands<CR>
  nmap <leader>c :Commands<CR>
  nmap é :History<CR>

  imap <c-x><c-f> <plug>(fzf-complete-path)

  nmap <leader>E :NERDTreeToggle<CR>
  nmap <leader>e :NERDTreeFind<CR>

  " TMUX Pane movement
  nnoremap <silent> <c-w><c-h> :TmuxNavigateLeft<CR>
  nnoremap <silent> <c-w>h :TmuxNavigateLeft<CR>
  nnoremap <silent> <c-w><c-j> :TmuxNavigateDown<CR>
  nnoremap <silent> <c-w>j :TmuxNavigateDown<CR>
  nnoremap <silent> <c-w><c-k> :TmuxNavigateUp<CR>
  nnoremap <silent> <c-w>k :TmuxNavigateUp<CR>
  nnoremap <silent> <c-w><c-l> :TmuxNavigateRight<CR>
  nnoremap <silent> <c-w>l :TmuxNavigateRight<CR>

  nmap <leader>a= :Tabularize /=<CR>
  vmap <leader>a= :Tabularize /=<CR>
  nmap <leader>a: :Tabularize /:\zs<CR>
  vmap <leader>a: :Tabularize /:\zs<CR>

  nmap <leader>ap <Plug>(ale_previous_wrap)
  nmap <leader>an <Plug>(ale_next_wrap)

  nnoremap <space> za

  tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>

" }}}

" Misc ----------------------------------------------------------------------{{{

  let g:ale_linters = {
  \   'javascript': ['eslint'],
  \   'typescript': ['tslint'],
  \}
  " let g:ale_sign_error = ' '
  " let g:ale_sign_error = ' '
  " let g:ale_sign_error = ' '
  " let g:ale_sign_warning = ' '
  " let g:ale_sign_error = ' '
  " let g:ale_sign_warning = ' '
  let g:ale_sign_warning = ' '
  let g:ale_sign_error = ' '
  let g:ale_javascript_eslint_executable = './node_modules/.bin/eslint'
  let g:EditorConfig_exclude_patterns = ['fugitive://.*']
  let g:doorboy_nomap_quotations = {
    \ 'javascript': ['/']
    \ }
  let g:localvimrc_whitelist=['/Users/tinimini/projects/', '/Users/tinimini/work/']
  let g:localvimrc_sandbox=0
  let g:indentLine_enabled=0
  " let g:indentLine_char='┆'
  let g:indentLine_char=''

  autocmd FileType typescript JsPreTmpl html
  " autocmd FileType typescript syn clear foldBraces
  autocmd FileType qf,fzf setlocal nonumber norelativenumber
  autocmd FileType html,css,scss,javascript,typescript,coffee,jsx,elm :IndentLinesEnable
  " autocmd FileType coffee setlocal suffixesadd=.js,.coffee,.ejs
  " autocmd FileType javascript setlocal suffixesadd=.js,.coffee,.ejs
  " autocmd FileType javascript.jsx setlocal suffixesadd=.js,.coffee,.ejs

" }}}

" Custom Functions ----------------------------------------------------------{{{

  function! CustomFind() " {{{
    execute 'Find' input('Whaddya want? ')
  endfunction " }}}

  function! CloseAllBuffersButCurrent() " {{{
    let curr = bufnr("%")
    let last = bufnr("$")

    if curr > 1    | silent! execute "1,".(curr-1)."bd"     | endif
    if curr < last | silent! execute (curr+1).",".last."bd" | endif
  endfunction " }}}

" }}}

" Folding -------------------------------------------------------------------{{{

  function! MyFoldText() " {{{
    let line=getline(v:foldstart)
    let nucolwidth=&fdc + &number * &numberwidth
    let windowwidth=winwidth(0) - nucolwidth - 3
    let foldedlinecount=v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab=strpart('          ', 0, &tabstop)
    let line=substitute(line, '\t', onetab, 'g')

    let line=strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount=windowwidth - len(line)
    return line . '…' . repeat(" ",fillcharcount)
  endfunction " }}}

  set foldtext=MyFoldText()

  autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
  autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

  autocmd FileType vim setlocal fdc=1
  set foldlevel=99

  autocmd FileType vim,tmux setlocal foldmethod=marker
  autocmd FileType vim setlocal foldlevel=0

  autocmd FileType javascript,html,css,scss,typescript setlocal foldlevel=99

  autocmd FileType css,scss,json setlocal foldmethod=marker
  autocmd FileType css,scss,json setlocal foldmarker={,}

  autocmd FileType coffee setl foldmethod=indent
  autocmd FileType xml setl foldmethod=syntax

  autocmd FileType html setl foldmethod=expr
  autocmd FileType html setl foldexpr=HTMLFolds()

  " autocmd FileType javascript,typescript,json setl foldmethod=syntax

" }}}

" Airline -------------------------------------------------------------------{{{

  if !exists('g:airline_symbols')
    let g:airline_symbols={}
  endif
  let g:airline#extensions#tabline#enabled=1
  let g:airline#extensions#mike#enabled=0
  set hidden
  let g:airline#extensions#tabline#fnamemod=':t'
  let g:airline#extensions#tabline#buffer_idx_mode=1
  let g:airline_powerline_fonts=1
  let g:airline#extensions#ale#error_symbol=' '
  let g:airline#extensions#ale#warning_symbol=' '
  let g:airline_symbols.branch=''
  let g:airline_theme='nord'

  nmap <leader>1 <Plug>AirlineSelectTab1
  nmap <leader>2 <Plug>AirlineSelectTab2
  nmap <leader>3 <Plug>AirlineSelectTab3
  nmap <leader>4 <Plug>AirlineSelectTab4
  nmap <leader>5 <Plug>AirlineSelectTab5
  nmap <leader>6 <Plug>AirlineSelectTab6
  nmap <leader>7 <Plug>AirlineSelectTab7
  nmap <leader>8 <Plug>AirlineSelectTab8
  nmap <leader>9 <Plug>AirlineSelectTab9

  let g:airline#extensions#tabline#buffer_idx_format={
        \ '0': '0 ',
        \ '1': '1 ',
        \ '2': '2 ',
        \ '3': '3 ',
        \ '4': '4 ',
        \ '5': '5 ',
        \ '6': '6 ',
        \ '7': '7 ',
        \ '8': '8 ',
        \ '9': '9 ',
        \}

" }}}

" NERDTree ------------------------------------------------------------------{{{

  let g:NERDTreeWinSize=45
  let g:NERDTreeQuitOnOpen=1
  let g:NERDTreeAutoDeleteBuffer=1
  let NERDTreeMinimalUI=1
  let NERDTreeDirArrows=1
  let g:WebDevIconsOS='Darwin'

"}}}

" FZF -----------------------------------------------------------------------{{{

  let $FZF_DEFAULT_COMMAND='ag -g ""'

  let g:fzf_buffers_jump=1
  let g:fzf_layout={'down': '~40%'}
  let g:fzf_colors=
        \ { 'fg':      ['fg', 'Normal'],
        \ 'bg':      ['bg', 'Normal'],
        \ 'hl':      ['fg', 'Comment'],
        \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
        \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
        \ 'hl+':     ['fg', 'Statement'],
        \ 'info':    ['fg', 'PreProc'],
        \ 'prompt':  ['fg', 'Conditional'],
        \ 'pointer': ['fg', 'Exception'],
        \ 'marker':  ['fg', 'Keyword'],
        \ 'spinner': ['fg', 'Label'],
        \ 'header':  ['fg', 'Comment'] }

  command! -bang -nargs=* Find
        \ call fzf#vim#grep(
        \   'rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1,
        \   <bang>0)

  command! -bang -nargs=* Ag
        \ call fzf#vim#grep(<q-args>,
        \   '--color-path 32 --color-line-number 34',
        \   <bang>0 ? fzf#vim#with_preview('up:60%')
        \           : fzf#vim#with_preview('right:50%:hidden', '?'),
        \   <bang>0)

  command! -bang -nargs=* Rg
        \ call fzf#vim#grep(
        \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
        \   <bang>0 ? fzf#vim#with_preview('up:60%')
        \           : fzf#vim#with_preview('right:50%:hidden', '?'),
        \   <bang>0)

  command! -bang -nargs=? -complete=dir Files
        \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

"}}}

" EasyMotion ----------------------------------------------------------------{{{

  let g:EasyMotion_startofline = 0

"}}}

" JavaScript/Typescript -----------------------------------------------------{{{

  let g:javascript_plugin_flow = 1
  let g:jsx_ext_required = 0
  " let g:ale_lint_on_save = 1
  " let g:ale_lint_on_text_changed = 0

  " Workaround for https://github.com/neoclide/vim-jsx-improve/issues/6
  autocmd BufNewFile,BufRead *.js,*.jsx setlocal ft=html ft=javascript

  autocmd BufNewFile,BufRead *.ejs setlocal ft=html

  " autocmd BufNewFile,BufRead *.ts,*.tsx set syntax=javascript

"}}}

" Python --------------------------------------------------------------------{{{

  let g:SimpylFold_docstring_preview=1

  au BufNewFile,BufRead *.py,*.elm set tabstop=4 softtabstop=4 shiftwidth=4

  "python with virtualenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF

"}}}

" UltiSnips -----------------------------------------------------------------{{{

  " Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
  let g:UltiSnipsExpandTrigger="<tab>"
  let g:UltiSnipsJumpForwardTrigger="<c-b>"
  let g:UltiSnipsJumpBackwardTrigger="<c-z>"
  let g:UltiSnipsSnippetsDir="~/.config/nvim/UltiSnips"

  " If you want :UltiSnipsEdit to split your window.
  let g:UltiSnipsEditSplit="vertical"

  autocmd FileType coffee UltiSnipsAddFiletypes coffeescript.javascript

" }}}

" Completion/Deoplete -------------------------------------------------------{{{
  set completeopt=longest,menuone,preview

  let g:deoplete#enable_at_startup = 1
  if !exists('g:deoplete#omni#input_patterns')
    let g:deoplete#omni#input_patterns = {}
  endif
  let g:tern#command = ["tern"]
  let g:tern#arguments = ["--persistent"]
  let g:tern_show_argument_hints = 'on_hold'
  let g:tern_show_signature_in_pum = 1
  let g:echodoc_enable_at_startup=1
  let g:deoplete#omni#functions = {}
  let g:deoplete#omni#functions.javascript = [
    \ 'tern#Complete'
  \]
  let g:deoplete#sources = {}
  let g:deoplete#sources['javascript.jsx'] = ['omni', 'buffer', 'member', 'ultisnips', 'ternjs', 'file']

  autocmd CompleteDone * pclose

  call deoplete#custom#source('buffer', 'mark', 'ℬ')
  call deoplete#custom#source('ternjs', 'mark', '')
  call deoplete#custom#source('omni', 'mark', '⌾')
  call deoplete#custom#source('file', 'mark', 'file')
  call deoplete#custom#source('jedi', 'mark', '')
  call deoplete#custom#source('typescript', 'mark', '')
  call deoplete#custom#source('neosnippet', 'mark', '')
  call deoplete#custom#source('typescript',  'rank', 630)
  function! Preview_func()
    if &pvw
      setlocal nonumber norelativenumber
     endif
  endfunction
  autocmd WinEnter * call Preview_func()
  " let g:deoplete#ignore_sources = {}
  " let g:deoplete#ignore_sources._ = ['around']

  function! g:Multiple_cursors_before()
    let g:deoplete#disable_auto_complete = 1
  endfunction
  function! g:Multiple_cursors_after()
    let g:deoplete#disable_auto_complete = 0
  endfunction

"}}}

" Emmet ---------------------------------------------------------------------{{{

  let g:user_emmet_mode='a'
  let g:user_emmet_complete_tag = 0
  let g:user_emmet_install_global = 0

  autocmd FileType html,css,scss EmmetInstall

" }}}

" TMUX ----------------------------------------------------------------------{{{

  let g:tmux_navigator_disable_when_zoomed=1

" }}}

" CtrlSF --------------------------------------------------------------------{{{

  let g:ctrlsf_ackprg = 'ag'
  let g:ctrlsf_mapping = {
      \ "next": "",
      \ "prev": "",
      \ }

" }}}
