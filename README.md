```sh
ln -s $PWD/gitconfig ~/.gitconfig
mkdir ~/.xmonad
ln -s $PWD/xmonad/xmonad.hs ~/.xmonad/
ln -s $PWD/xmonad/DBus-spotify.py ~/.xmonad/
mkdir -p ~/.config/fish/
ln -s $PWD/fish/own_functions  ~/.config/fish/
ln -s $PWD/fish/completions  ~/.config/fish/completions
ln -s $PWD/fish/solarized.fish  ~/.config/fish/
ln -s $PWD/fish/config.fish  ~/.config/fish/
ln -s $PWD/fish/nvm-wrapper  ~/.config/fish/
mkdir ~/bin/
ln -s $PWD/bin/gw ~/bin/
ln -s $PWD/Xmodmap ~/.Xmodmap
ln -s $PWD/vimrc ~/.vimrc
ln -s $PWD/xsessionrc ~/.xsessionrc
mkdir -p ~/.config/inkscape/palettes/
ln -s $PWD/inkscape-palettes/ercis.gpl ~/.config/inkscape/palettes/
sudo ln -s $PWD/sysfiles/sleep_d_00_pidgin /etc/pm/sleep.d/00_pidgin
# Mate Terminal: Neues Profil von Hand anlegen!!; Terminal neu starten
cd mate-solarized
./install.sh
cd ../bass
make install
```

