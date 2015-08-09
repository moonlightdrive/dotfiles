xrdb -merge ~/.Xresources
xsetroot Vanilla-DMZ-aa left_ptr
setxkbmap dvorak
xbindkeys -f .config/xbindkeysrc
compton -b
#emacs --daemon
unclutter &
sh ~/.fehbg &
exec xmonad
