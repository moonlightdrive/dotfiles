#
# ~/.bashrc
# read and executed by Bash every time a subshell is started
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# XXX okay I have these but what am I doing with them?
#SWITCH=$(opam switch list | awk '/C/{print $1}')
#GITBRANCH=$(git branch | awk '/*/{print $2}')

## TODO want to be able to color PS1 iff PS1 > x chars. doable?
PS1='[\u@\h \W]\$ '

set -o emacs

# for colored ls in termite
eval $(dircolors ~/.dircolors)

# bash vars http://tldp.org/LDP/abs/html/internalvariables.html

# Aliases
alias mkpkg=makepkg
alias e=$EDITOR
alias touche='touch' # can't stop typing this. gave up. alias'd.
alias ls='ls --color=auto'
alias ll='ls -la'
alias l.='ls -d .*' # show hidden files
# alias R='R --quiet'

# TODO get rid of all these programs pls
alias mml="wine $HOME/.wine/drive_c/Program\ Files\ \(x86\)/Multimedia\ Logic/Mmlogic.exe"
#alias lc3convert="$HOME/bin/lc3tools/lc3convert"
#alias lc3sim="$HOME/bin/lc3tools/lc3sim-tk"

alias mc='mv $@ && cd $_'
alias cl=cdls
alias dev='cd $HOME/dev/$1'

cdls() { cd "$@" && ls; }

# OPAM configuration
. /home/moonlight/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

