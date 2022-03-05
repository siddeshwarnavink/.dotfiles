" Section: Bootstrap {{{
if filereadable($VIMRUNTIME.'/defaults.vim')
  source $VIMRUNTIME/defaults.vim
endif

let mapleader = ','
let maplocalleader = ';'

set nocompatible
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
set wildignore=*.o,*.class
set omnifunc=syntaxcomplete#Complete
set completefunc=syntaxcomplete#Complete
set pastetoggle=<F2>
set background=light
set laststatus=0
set ruler
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

nnoremap <tab>h <C-w>h
nnoremap <tab>j <C-w>j
nnoremap <tab>k <C-w>k
nnoremap <tab>l <C-w>l
nnoremap <silent> <tab><tab> :b#<CR>

nnoremap <silent> <M-Left> :vertical resize +2<CR>
nnoremap <silent> <M-Right> :vertical resize -2<CR>
nnoremap <silent> <M-Up> :resize +1<CR>
nnoremap <silent> <M-Down> :resize -1<CR>

nnoremap <leader>p "+p
imap <leader>p <C-r>+
cmap <leader>p <C-r>+

" indent current paragraph
nnoremap <leader>= mz=ap`z

nnoremap <silent> [b :bprev<CR>
nnoremap <silent> ]b :bnext<CR>
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

nnoremap <silent> <leader>w :write<CR>
nnoremap <silent> <leader>W :wall<CR>
nnoremap <silent> <leader>q :quit!<CR>
nnoremap <silent> <leader>Q :quitall!<CR>
nnoremap <silent> <leader>o :only<CR>
nnoremap <silent> <leader>z :setlocal spell!<CR>

" duplicate line
nnoremap <silent> <leader>d :let c = getpos(".")<CR>Yp:call setpos(".", [c[0], c[1] + 1, c[2], c[3]])<CR>

" replace word under cursor
nnoremap <leader>s /\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgn
nnoremap <leader>S :%s/\<<C-r><C-w>\>/

" grep word under cursor
nnoremap <leader>* :execute 'grep! ' . expand('<cword>') . ' **/*'<CR>:copen<CR>

if has('terminal')
  " nnoremap <silent> <leader>xx :tab term<CR>"
  nnoremap <silent> <leader>xx :term ++curwin<CR>
  nnoremap <silent> <leader>xb :bot term<CR>

  tnoremap <Esc><Esc> <C-\><C-n>
  tmap <leader>p <C-W>"+

  tnoremap <tab>h <C-w>h
  tnoremap <tab>j <C-w>j
  tnoremap <tab>k <C-w>k
  tnoremap <tab>l <C-w>l
  tnoremap <silent> <tab><tab> <C-w>:b#<CR>

  tnoremap <silent> <M-Left> <C-\><C-n>:vertical resize +2<CR>
  tnoremap <silent> <M-Right> <C-\><C-n>:vertical resize -2<CR>
  tnoremap <silent> <M-Up> <C-\><C-n>:resize +1<CR>
  tnoremap <silent> <M-Down> <C-\><C-n>:resize -1<CR>
endif
" }}}
" Section: Plugins {{{
let g:netrw_banner = 0
let g:netrw_list_hide='\.swp$'

nnoremap <silent> - :Explore<CR>

function! NetrwMapping()
  nmap <buffer> <space> mf
  nmap <buffer> <S-space> mu
  nmap <buffer> m? :echo join(netrw#Expose("netrwmarkfilelist"), "\n")<CR>
  nmap <buffer> t? :echo 'Target:' . netrw#Expose("netrwmftgt")<CR>
endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END

if !exists('g:loaded_matchit')
  runtime! macros/matchit.vim
endif

if !exists('g:loaded_man')
  runtime ftplugin/man.vim
endif

if has('packages')
  silent! helptags ALL
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
