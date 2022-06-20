set nocompatible              " be iMproved, required
" filetype off                  " required

" set the runtime path to include Vundle and initialize
" alternatively, pass a path where Vundle should install plugins


"   PLUGINS

call plug#begin('~/.vim/plugged')

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
" Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" Multiple Plug commands can be written in a single line using | separators
" Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
" Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
" Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Using a non-master branch
" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
" Plug 'fatih/vim-go', { 'tag': '*' }

" Plugin options
" Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }

" Plugin outside ~/.vim/plugged with post-update hook
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" Unmanaged plugin (manually installed and updated)
" Plug '~/my-prototype-plugin'

" Plug 'preservim/nerdtree'

Plug 'vim-airline/vim-airline'

Plug 'vim-airline/vim-airline-themes'

Plug 'fatih/vim-go'

Plug 'dense-analysis/ale'

Plug 'majutsushi/tagbar'

Plug 'ajh17/VimCompletesMe'

Plug 'slashmili/alchemist.vim'

Plug 'elixir-editors/vim-elixir'

Plug 'slashmili/alchemist.vim'

Plug 'tpope/vim-endwise'

Plug 'vimlab/split-term.vim'

Plug 'NLKNguyen/vim-lisp-syntax'

Plug 'dart-lang/dart-vim-plugin'

Plug 'vlime/vlime', {'rtp': 'vim/'}

" Initialize plugin system
call plug#end()

" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList          - list configured plugins
" :PluginInstall(!)    - install (update) plugins
" :PluginSearch(!) foo - search (or refresh cache first) for foo
" :PluginClean(!)      - confirm (or auto-approve) removal of unused plugins
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


"   OPTIONS

set autoindent                  " Carry over indenting from previous line
set autoread                    " Don't bother me when a file changes
set autowrite                   " Write on :next/:prev/^Z
set backspace=indent,eol,start  " Allow backspace beyond insertion point
set copyindent              " Make autoindent use the same chars as prev line
set directory-=.            " Don't store temp files in cwd
set encoding=utf8           " UTF-8 by default
set expandtab               " No tabs
set formatoptions=tcqn1     " t - autowrap normal text
                            " c - autowrap comments
                            " q - gq formats comments
                            " n - autowrap lists
                            " 1 - break _before_ single-letter words
                            " 2 - use indenting from 2nd line of para
set hlsearch                " Hilight searching
set incsearch               " Search as you type
set infercase               " Completion recognizes capitalization
set laststatus=2            " Always show the status bar
set linebreak               " Break long lines by word, not char
set tabstop=2               " The One True Tab
set textwidth=100           " 100 is the new 80

set wildmenu


" Start NERDTree on Vim starts in directory
" autocmd vimenter * NERDTree

" Start NERDTree on Vim starts regardless of if you're in a directory
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Change default NERDTree arrows
" let g:NERDTreeDirArrowExpandable = '▸'
" let g:NERDTreeDirArrowCollapsible = '▾'

" Essential for filetype plugins.
filetype plugin indent on

" Set splits to be below and right
set splitbelow
set splitright


" Lisp things
" set lines=24 columns=80
" set shiftwidth=2
" set syntax=lisp

"  ALE
" let g:ale_linters = {
"   'elixir': ['elixir-ls'],
" }

" let g:ale_fixers = {
"   'elixir': ['mix_format'],
" }

" let g:ale_elixir_elixir_ls_release='~/elixir_stuff/elixir-ls'

let g:ale_completion_enabled = 1
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_lint_on_enter = 0
let g:ale_lint_on_text_changed = 'never'
highlight ALEErrorSign ctermbg=NONE ctermfg=red
highlight ALEWarningSign ctermbg=NONE ctermfg=yellow
let g:ale_linters_explicit = 1
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 1

" Netrw directory explorer
let g:netrw_liststyle = 3  " show the tree listing
let g:netrw_winsize = 0    " set default window size to be always equal
let g:netrw_preview = 1    " open splits to the right
"let g:netrw_banner = 0     " hide help banner on startup


" By default, netrw leaves unmodified buffers open. This autocommand
" deletes netrw's buffer once it's hidden (using ':q;, for example)
autocmd FileType netrw setl bufhidden=delete   " or use :qa!

" These three lines are for the fuzzy search:
set nocompatible       " Limit search to your project
set path+=**            " Search all subdirectories and recursively
set wildmenu            " Shows multiple matches on one line



noremap <Leader>ad :ALEGoToDefinition<CR>
nnoremap <leader>af :ALEFix<cr>
noremap <Leader>ar :ALEFindReferences<CR>

" Move between linting errors
nnoremap ]r :ALENextWrap<CR>
nnoremap [r :ALEPreviousWrap<CR>


" NERDTree MAPPINGS
" Switch split panes with <ctrl>+j/k/h/l
"map <C-j> <C-W>j
"map <C-k> <C-W>k
"map <C-h> <C-W>h
"map <C-l> <C-W>l




"   COLORS

syntax on

" Make sure colored syntax mode is on, and make it Just Work with 256-color terminals.
" set background=dark
" let g:rehash256 = 1 " alternative scheme
" let g:molokai_original = 1
" colorscheme molokai

"set background=dark
colorscheme onedark

" Custom colors for NERDTree
" highlight def link NERDTreeRO NERDTreeFile

" Make menu selections visible
highlight PmenuSel ctermfg=black ctermbg=magenta




" More sweet config at https://github.com/statico/dotfiles/blob/master/.vim/vimrc


