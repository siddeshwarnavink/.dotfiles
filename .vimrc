" Section: Bootstrap {{{
filetype plugin indent on
syntax on

let mapleader = ','
let maplocalleader = ';'

set nocompatible
set encoding=utf-8
set updatetime=300
set shortmess+=I
set exrc
set autoread autowrite
set hidden
set termguicolors
set number relativenumber
set splitright splitbelow
set confirm
set showmatch " 4lisp
set clipboard^=unnamed,unnamedplus
set backspace=indent,eol,start
set foldmethod=manual
set spelllang=en_us
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set hlsearch
set incsearch
let &undodir = $HOME . "/.vim/undodir"
set undofile
set path=**
set wildmenu
set wildignore=*.o,*.class,*/node_modules/*
set omnifunc=syntaxcomplete#Complete
set completefunc=syntaxcomplete#Complete
set pastetoggle=<F2>
set background=dark
set laststatus=2
set statusline=[%n]\ %<%.99f\ %y%h%w%m%r%=%-14.(%l,%c%V%)\ %P
set ruler
set redrawtime=10000 " handle big files
set mouse=a
" }}}
" Section: Remaps {{{
nnoremap <silent> <Esc> :nohl<CR>

" Emacs keys
cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-D> <Del>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-N> <Down>
cnoremap <C-P> <Up>
cnoremap <Esc><C-B>	<S-Left>
cnoremap <Esc><C-F>	<S-Right>

vnoremap < <gv
vnoremap > >gv

nnoremap <silent> <tab><tab> :b#<CR>

nnoremap <leader>p "+p
imap <leader>p <C-r>+
cmap <leader>p <C-r>+

" indent current paragraph
nnoremap <leader>= mz=ap`z

nnoremap <silent> <leader>bd :bdelete!<CR>
nnoremap <silent> <leader>bD :bufdo bdelete!<CR>e .<CR>

nnoremap <silent> [q :cprev<CR>zz
nnoremap <silent> ]q :cnext<CR>zz

function! ToggleQuickfix()
  if empty(filter(getwininfo(), 'v:val.quickfix'))
    copen
  else
    cclose
  endif
endfunction
nnoremap <silent> <leader>cc :call ToggleQuickfix()<CR>

vnoremap <silent> K :m '<-2<CR>gv=gv
vnoremap <silent> J :m '>+1<CR>gv=gv

nnoremap <silent> <leader>z :setlocal spell!<CR>
nnoremap <silent> <leader>l :setlocal list!<CR>

" duplicate line
nnoremap <silent> <leader>d :let c = getpos(".")<CR>Yp:call setpos(".", [c[0], c[1] + 1, c[2], c[3]])<CR>

" replace word under cursor
nnoremap <leader>s /\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgn
nnoremap <leader>S :%s/\<<C-r><C-w>\>/<C-r><C-w>

" grep word under cursor
if has('win32') || has('win64')
  set grepprg=grep\ -nH\ $*
  set grepformat=%f:%l:%m
  nnoremap <leader>* :execute 'silent! grep! ' . expand('<cword>') . ' *' <CR> :copen<CR>
else
  nnoremap <leader>* :execute 'silent! grep! "\<'.expand('<cword>').'\>" **/*'<CR>:copen<CR>
endif

" fix syntax in large codebase
noremap <silent> <F12> <Esc>:syntax sync fromstart<CR>
" }}}
" Section: Plugins {{{
let g:netrw_banner = 0
let g:netrw_keepdir = 0
let g:netrw_list_hide='\.swp$'

nnoremap <silent> - :Explore<CR>

function! NetrwMapping()
  nmap <buffer> <space> mf
  nmap <buffer> <S-space> mu
  nmap <buffer> m? :echo join(netrw#Expose("netrwmarkfilelist"), "\n")<CR>
  nmap <buffer> t? :echo 'Target:' . netrw#Expose("netrwmftgt")<CR>
  nmap <buffer> mC mtmc
  nmap <buffer> mM mtmm
endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END

hi! link netrwMarkFile Search

if !exists('g:loaded_matchit')
  runtime! macros/matchit.vim
endif

if !exists('g:loaded_man')
  runtime ftplugin/man.vim
endif

silent! colorscheme gruvbox

nnoremap <silent> <leader>xt :Dispatch! ctags -R .<CR>
autocmd FileType java let b:dispatch = 'javac %:S'

let g:projectionist_heuristics = {
      \   "*": {
      \     "src/*.c": {
      \      "type": "src",
      \       "alternate": "src/{}.h"
      \     },
      \     "src/*.h": {
      \      "type": "header",
      \       "alternate": "src/{}.c"
      \     },
      \   }
      \ }
nnoremap <silent> <leader>a :A<CR>

nnoremap <silent> <leader>gs :Git<CR>
nnoremap <silent> <leader>gd :Gdiffsplit<CR>

autocmd FileType c,cpp,java setlocal commentstring=//\ %s

let g:lsp_use_native_client = 1
let g:lsp_format_sync_timeout = 1000
let g:lsp_diagnostics_highlights_enabled = 0
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_diagnostics_virtual_text_enabled = 0

function! s:LspBufferEnable()
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> gs <plug>(lsp-document-symbol-search)
  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [d <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]d <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
  nnoremap <leader>cf :LspDocumentFormat<CR>
  nnoremap <leader>ca :LspCodeAction<CR>
endfunction

autocmd User lsp_buffer_enabled call s:LspBufferEnable()
" }}}
" Section: Misc {{{
autocmd InsertEnter * set nopaste
autocmd BufWritePre * :%s/\s\+$//e
autocmd BufNewFile,BufRead *.json set ft=json syntax=javascript
autocmd FileType markdown,text,gitcommit setlocal spell

command! SudoWrite execute 'w !sudo tee % > /dev/null' | edit!

if filereadable(expand('~/local.vim'))
  source ~/local.vim
endif
" }}}
" vi:set ts=2 sts=2 sw=2 et fdm=marker:
