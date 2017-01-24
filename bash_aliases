alias l.='ls -d .*'
alias mc='mv $@ && cd $_'
alias cl=cdls

cdls() { cd "$@" && ls; }