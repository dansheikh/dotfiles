[ -f $HOME/.Xresources ] && xrdb -merge $HOME/.Xresources

[ -f $HOME/.init-stumpwm.lisp ] && exec sbcl --load $HOME/.init-stumpwm.lisp
