cd Documents/programming/lisp
nvim-termite tutorial.lisp

ssh klingenberg@klingenbergServer

cd .dotfiles/dotfiles/binCustom/
nvim-termite ./.dotfiles/dotfiles/binCustom/nterm
nvim-termite ./INSTALL
ls
chmod +x nterm
cd ..
./INSTALL
xmonad --restart
man nvim

nvim-termite nvimshell/nvimshellrc.vim
nvim-termite .binlist

git status
git add binCustom/lfStandalone
git add desktopFiles/lfStandalone.desktop
git add nvimshell/nvimshell.sh
git add nvimshell/nvimshellrc.vim

cd nvim
ls
cd ./binCustom
cp rangerStandalone lfStandalone
nvim-termite lfStandalone
nvim-termite rangerStandalone
cd desktopFiles
cp rangerStandalone.desktop lfStandalone.desktop

