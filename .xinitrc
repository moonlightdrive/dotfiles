xrdb -merge ~/.Xresources
xsetroot Vanilla-DMZ-aa left_ptr
setxkbmap dvorak
xbindkeys -f .config/xbindkeysrc
compton -b
#emacs --daemon
sh ~/.fehbg &
exec xmonad
