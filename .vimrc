" Config for Vim 9
set number relativenumber
set tabstop=4 shiftwidth=4 softtabstop=4
set autoindent
set expandtab
set autowrite autoread
set incsearch

cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-D> <Del>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-N> <Down>
cnoremap <C-P> <Up>

nnoremap m<CR> :make<CR>
nnoremap [q :cprev<CR>
nnoremap ]q :cnext<CR>

nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

highlight ws ctermbg=red guibg=red
match ws /\s\+$/
