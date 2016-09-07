if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
            [ -x "$f" ] && . "$f"
                done
                    unset f
                    fi

xrdb -merge ~/.Xresources

xsetroot Vanilla-DMZ-aa left_ptr

setxkbmap dvorak
xbindkeys -f .config/xbindkeysrc
compton -fb
unclutter &
sh ~/.fehbg &
[[ -a $HOME/.Xmodmap ]] && /usr/bin/xmodmap $HOME/.Xmodmap && exec xmonad
