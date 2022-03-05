set guioptions-=m
set guioptions-=T
if has('win32')
  let &guifont = 'Consolas:h14,Courier New:h14'
  set shell=powershell
  set shellcmdflag=-Command
  set shellquote=\"
  set shellxquote=
else
  let &guifont = 'Monospace 14'
endif
highlight Terminal guibg=bg guifg=fg
" vi:set ts=2 sts=2 sw=2 et fdm=marker:
