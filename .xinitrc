# export XDG_SESSION_TYPE=x11
# export GDK_BACKEND=x11
# 
# exec gnome-session

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

xrdb ~/.Xresources

CUSTOM_WM=${1:-i3}

case $CUSTOM_WM in
	i3) exec i3;;
	berry) exec berry;;
	gala)
		sxhkd &
		exec gala
		;;
	mutter)
		sxkhd &
		exec mutter --replace
		;;
	xfwm4)
		sxkhd &
		exec xfwm4
		;;
	kwin)
		sxkhd &
		exec kwin_x11
		;;
	gnome)
		export XDG_SESSION_TYPE=x11
		export GDK_BACKEND=x11
		exec gnome-session
		;;
	kde|plasma)
		exec startplasma-x11
		;;
	awesome)
		exec awesome
		;;
	spectrwm)
		exec spectrwm
		;;
	bspwm)
		exec bspwm
		;;
	xmonad)
		exec xmonad
		;;
esac
