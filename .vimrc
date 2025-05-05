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

let mapleader = ','
let maplocalleader = ';'

cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-D> <Del>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-N> <Down>
cnoremap <C-P> <Up>

vnoremap <silent> K :m '<-2<CR>gv=gv
vnoremap <silent> J :m '>+1<CR>gv=gv

nnoremap [q :cprev<CR>zz
nnoremap ]q :cnext<CR>zz

nnoremap <silent> <tab><tab> :b#<CR>
nnoremap <leader>= mz=ap`z
nnoremap <silent> <leader>d :let c = getpos(".")<CR>Yp:call setpos(".", [c[0], c[1] + 1, c[2], c[3]])<CR>

nnoremap <leader>y :let g:yanked_path = expand('%:p')<CR>
function! PasteRelativePath()
  if !exists('g:yanked_path') || empty(g:yanked_path)
    echo "No path yanked"
    return
  endif

  let l:from = expand('%:p:h')
  let l:to = g:yanked_path

  let l:from_list = split(fnamemodify(l:from, ':p'), '/')
  let l:to_list = split(fnamemodify(l:to, ':p'), '/')

  let l:i = 0
  while l:i < len(l:from_list) && l:i < len(l:to_list) && l:from_list[l:i] ==# l:to_list[l:i]
    let l:i += 1
  endwhile

  let l:rel = repeat(['..'], len(l:from_list) - l:i)
  let l:rel += l:to_list[l:i :]

  let l:relpath = join(l:rel, '/')
  execute "normal! a" . l:relpath
endfunction
nnoremap <leader>p :call PasteRelativePath()<CR>

nnoremap <leader>s /\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgn
nnoremap <leader>* :execute 'silent! grep! "\<'.expand('<cword>').'\>" **/*'<CR>:copen<CR>

function! ToggleQuickfix()
  if empty(filter(getwininfo(), 'v:val.quickfix'))
    copen
  else
    cclose
  endif
endfunction
nnoremap <silent> <leader>cc :call ToggleQuickfix()<CR>

nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
noremap <silent> <F12> <Esc>:syntax sync fromstart<CR>

if !exists('g:loaded_man')
  runtime ftplugin/man.vim
endif

if !exists('g:loaded_matchit')
  runtime! macros/matchit.vim
endif

let g:netrw_banner = 0
let g:netrw_list_hide='\.swp$'

nnoremap <silent> - :Explore<CR>

silent! colorscheme gruvbox

highlight ws ctermbg=red guibg=red
match ws /\s\+$/

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

nnoremap <silent> <leader>gs :Git<CR>
nnoremap <silent> <leader>gd :Gdiffsplit<CR>

let g:lsp_format_sync_timeout = 1000
let g:lsp_diagnostics_highlights_enabled = 0
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_diagnostics_virtual_text_enabled = 0

function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> gs <plug>(lsp-workspace-symbol-search)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [d <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]d <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
  nnoremap <buffer> <leader>cf :call execute('LspDocumentFormatSync')<CR>
endfunction

augroup lsp_install
  au!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

" vi:set ts=2 sts=2 sw=2 et:
