" Config for Vim 9
silent! source $VIMRUNTIME/defaults.vim

set termguicolors
set laststatus=2
set background=dark
set number relativenumber
set tabstop=4 shiftwidth=4 softtabstop=4
set autoindent
set expandtab
set autowrite autoread
set hidden
set incsearch
set belloff=all

cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-D> <Del>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-N> <Down>
cnoremap <C-P> <Up>

nnoremap [q :cprev<CR>zz
nnoremap ]q :cnext<CR>zz

nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

silent! colorscheme gruvbox

highlight ws ctermbg=red guibg=red
match ws /\s\+$/

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

let g:NERDTreeMinimalUI = 1
let g:NERDTreeShowHidden=1
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

autocmd BufEnter * if (winnr("$") == 1 && &filetype == "nerdtree") | quit | endif

if !exists('g:loaded_man')
  runtime ftplugin/man.vim
endif

" vi:set ts=2 sts=2 sw=2 et:
