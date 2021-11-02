" NERDTree show hidden files: shift + i
" show filetype: :set ft?
call plug#begin('~/.config/nvim/plugged')
Plug 'preservim/nerdtree'
Plug 'ghifarit53/tokyonight-vim'
call plug#end()

set termguicolors

let g:tokyonight_style = 'night' " available: night, storm
let g:tokyonight_enable_italic = 1

colorscheme tokyonight

" noremap ; l
" noremap l k
" noremap k j
" noremap j h

filetype plugin indent on
set nocompatible
syntax on
set number

set smartindent
set autoindent

nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <C-u> :w <bar> :!refer -PS -e -p ~/Bibliography.txt % <bar> groff -et -ms -Tpdf > output.pdf<CR>
nnoremap <C-i> :w <bar> :!pdftotext output.pdf - <bar> tr -d '.' <bar> wc -w<CR>
nnoremap <C-p> :w <bar> :!pdflatex %<CR>

" Exit Vim if NERDTree is the only window left.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif
