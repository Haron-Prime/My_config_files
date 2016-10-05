"! /bin/vim
if v:progname =~? "evim"
  finish
endif

runtime! archlinux.vim

set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file (restore to previous version)
  set undofile		" keep an undo file (undo changes after closing)
endif

set backupcopy=auto 
set swapfile
set swapsync=fsync
set undolevels=1000                                 " 50 undos - saved in undodir
set udf
set udir=$HOME/.vim/undofiles//
set directory=$HOME/.vim/swp//
set modeline
set statusline=%f%m%r%h%w\ %y\ enc:%{&enc}\ ff:%{&ff}\ fenc:%{&fenc}%=(ch:%3b\ hex:%2B)\ col:%2c\ line:%2l/%L\ [%2p%%]
set title titlelen=150 titlestring=%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}
set ttyfast                                         " we have a fast terminal
set scrolljump=5                                    " when scrolling up down, show at least 5 lines
set ttyscroll=999                                   " make vim redraw screen instead of scrolling when there are more than 3 lines to be scrolled
set updatecount=250                                 " switch every 250 chars, save swap
set wrap
set linebreak
set list                                            " list disables linebreak
set textwidth=0
set wrapmargin=0
set whichwrap+=b,s,<,>,h,l,[,]                      " backspaces and cursor keys wrap to
set wildmenu
set autoindent smartindent                          " auto/smart indent
set autoread                                        " watch for file changes
set cmdheight=1                                     " command line two lines high
set complete=.,w,b,u,U,t,i,d                        " do lots of scanning on tab completion
set cursorline                                      " show the cursor line
set encoding=utf-8                                  " set charset translation encoding
set termencoding=utf-8                              " set terminal encoding
set fileencoding=utf-8                              " set save encoding
set fileencodings=utf8,koi8r,cp1251,cp866,ucs-2le   " список предполагаемых кодировок, в порядке предпочтения
set history=3000                                    " keep 3000 lines of command line history
set keywordprg=TERM=mostlike\ man\ -s\ -Pless
set laststatus=2
set linebreak                                       " wrap at 'breakat' instead of last char
set magic                                           " Enable the "magic"
set maxmem=25123                                    " 24 MB -  max mem in Kbyte to use for one buffer.  Max is 2000000
set noautowrite                                     " don't automagically write on :next
set noexpandtab                                     " no expand tabs to spaces"
set nohidden                                        " close the buffer when I close a tab (I use tabs more than buffers)
"set noerrorbells visualbell t_vb= " Disable ALL bells
set number                                          " line numbers
"set pastetoggle=<F11>
set scrolloff=3                                     " keep at least 3 lines above/below
set shiftwidth=3                                    " shift width
set showcmd                                         " Show us the command we're typing
set showfulltag                                     " show full completion tags
set showmode                                        " show the mode all the time
set sidescroll=2                                    " if wrap is off, this is fasster for horizontal scrolling
set sidescrolloff=2                                 "keep at least 5 lines left/right
set noguipty
set splitright
set splitbelow
set restorescreen=on                                " restore screen contents when vim exits -  disable withset t_ti= t_te=
set winheight=25
set equalalways                                     " all the windows are automatically sized same
set eadirection=both                                " only equalalways for horizontally split windows
set tabstop=4
set softtabstop=4
set shiftwidth=3
set switchbuf=usetab
set commentstring=#%s
set tabpagemax=150
set showtabline=1                                   " 2 always, 1 only if multiple tabs
set smarttab                                        " tab and backspace are smart
" set foldmethod=syntax
" set foldenable
" set foldcolumn=1                                    " the blank left-most bar left of the numbered lines
set sc                                              " override 'ignorecase' when pattern has upper case characters
set smartcase                                       " Ignore case when searching lowercase
set showmatch                                       " show matching bracket
set diffopt=filler,iwhite                           " ignore all whitespace and sync"
set stal=2
" set viewoptions=folds,localoptions,cursor
set ruler                                           " show the cursor position all the time
set showcmd                                         " display incomplete commands
set incsearch                                       " do incremental searching
set hlsearch
set guiheadroom=0

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" let loaded_vifm=1

let g:indentLine_color_term = 239
let g:indentLine_enabled = 1
" let g:indentLine_char = '│'

let g:syntastic_always_populate_loc_list = 3
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" let g:syntastic_haskell_ghc_mod_exec = 'ghc-mod.sh'

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

if has('langmap') && exists('+langnoremap')
  " Prevent that the langmap option applies to characters that result from a
  " mapping.  If unset (default), this may break plugins (but it's backward
  " compatible).
  set langnoremap
endif


" настройки Vundle
set rtp+=~/.vim/bundle/vundle/

call vundle#rc()

"репозитории на github
" Bundle 'tpope/vim-fugitive'
Bundle 'lokaltog/vim-easymotion'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}

"репозитории vim/scripts
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'rails.vim'
Bundle 'chilicuil/vim-sprunge'

"git репозитории (не на github)
"Bundle 'git://git.wincent.com/command-t.git'

"плагины
Plugin 'bling/vim-bufferline'
" Plugin 'MattesGroeger/vim-bookmarks'
Plugin 'mattn/emmet-vim'
" Plugin 'aperezdc/vim-template'
" Plugin 'int3/vim-extradite'
" Plugin 'vim-scripts/gitv'
Plugin 'sjl/gundo.vim'
Plugin 'vim-scripts/netrw.vim'
" Plugin 'vim-scripts/sudo.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tomtom/tcomment_vim'
" Plugin 'vim-scripts/toggle_words.vim'
Plugin 'scrooloose/syntastic'
" Plugin 'terryma/vim-multiple-cursors'
" Plugin 'airblade/vim-gitgutter'
Plugin 'Yggdroot/indentLine'
Plugin 'vim-scripts/sessionman.vim'
Plugin 'prurigro/vim-archversion'
Plugin 'Matt-Deacalion/vim-systemd-syntax'
" Plugin 'xolox/vim-easytags'
Plugin 'xolox/vim-misc'
Plugin 'jamessan/vim-gnupg'
Plugin 'vim-scripts/pacmanlog.vim'
" Plugin 'thisivan/vim-taglist'
Plugin 'vim-scripts/open-browser.vim'
Plugin 'groenewege/vim-less'
" Plugin 'guns/xterm-color-table.vim'
Plugin 'jeffkreeftmeijer/vim-numbertoggle'
" Plugin 'vim-scripts/Tabstuff'

"My settings

filetype on

set guiheadroom=0
set t_Co=256
set showmatch
set list lcs=tab:>-,space:.
set showcmd 

color antares

let Tlist_Compact_Format = 1
let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Close_On_Select = 1
nnoremap <C-l> :TlistToggle<CR>

"let g:extradite_width=40

let g:user_emmet_mode='a'

" autocmd VimLeavePre * silent mksession $HOME/.vim/lastSession.vim

if has("autocmd")
        au BufNewFile,BufRead * if getline(1) =~ '^#!.*iptables-restore' ||
                \ getline(1) =~ '^# Generated by iptables-save' ||
                \ getline(1) =~ '^# Firewall configuration written by' |
                \ set ft=iptables | endif
endif


"let g:bookmark_sign = '♥'
let g:bookmark_sign = 'β'
let g:bookmark_auto_save = 1
let g:bookmark_manage_per_buffer = 1
let g:bookmark_auto_save_file = '/bookmarks'

let &keywordprg=':help'

" set list lcs=tab:\|\ 
let g:gundo_width = 60
let g:gundo_preview_height = 40
let g:gundo_right = 1

set backup

" сохранять умные резервные копии ежедневно
function! BackupDir()
	" определим каталог для сохранения резервной копии
	let l:backupdir=$HOME.'/.vim/backup/'.
			\substitute(expand('%:p:h'), '^'.$HOME, '~', '')

	" если каталог не существует, создадим его рекурсивно
	if !isdirectory(l:backupdir)
		call mkdir(l:backupdir, 'p', 0700)
	endif

	" переопределим каталог для резервных копий
	let &backupdir=l:backupdir

	" переопределим расширение файла резервной копии
	let &backupext=strftime('~%Y-%m-%d~')
endfunction

" выполним перед записью буффера на диск
autocmd! bufwritepre * call BackupDir()

"" Удалить пробелы в конце строк (frantsev)
function! RemoveTrailingSpaces()
   normal! mzHmy
   execute '%s:\s\+$::ge'
   normal! 'yzt`z
endfunction

let g:ycm_filetype_specific_completion_to_disable = {
      \ 'gitcommit': 1
      \}

let g:NumberToggleTrigger="<S-M>"

"My keybinding

"inoremap { {<CR>}<Esc>O
nmap      <F2>         :qa<CR>
nmap      <F3>         <Esc>:q<CR>
imap      <F3>         <Esc>:wq<CR>
imap      <F4>         <Esc>:w<CR>
nmap      <F4>         :w<CR>
map       <F5>         :NERDTreeToggle<CR>
nnoremap  <F6>         :GundoToggle<CR>
nmap      <F7>         :r! xclip -o<CR>
imap      <F7>         <Esc>:r! xclip -o<CR>
nmap      <F8>         <Esc>:browse tabnew<CR>
imap      <F8>         <Esc>:browse tabnew<CR>
nmap      <F9>         <Plug>(openbrowser-open)
vmap      <F9>         <Plug>(openbrowser-open)
nmap      <F10>        <Plug>(openbrowser-search)
vmap      <F10>        <Plug>(openbrowser-search)
nmap      <F11>        :SessionList<CR>
map       <S-Insert>   <MiddleMouse>
map!      <S-Insert>   <MiddleMouse>
nnoremap  <space>      za
map       <C-C>        gcc
nmap      <S-T>        :ToggleWord<CR>
" nmap      <S-S>        :source $HOME/.vimrc<CR>
map       <S-A>        :IndentLinesToggle<CR>
nmap      <S-S>        :SessionSave<CR>
nmap      <S-X>        :SessionSaveAs<CR>
nmap      cv           yy:silent .w !xclip&lt;<CR>
vmap      c            y:silent '<,'> w !xclip&lt;<CR>
