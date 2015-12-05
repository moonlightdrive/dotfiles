xrdb -merge ~/.Xresources

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

xsetroot Vanilla-DMZ-aa left_ptr
setxkbmap dvorak
xbindkeys -f .config/xbindkeysrc
# compton -fb
unclutter &
sh ~/.fehbg &
exec xmonad
