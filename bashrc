#
# ~/.bashrc
# read and executed by Bash every time a subshell is started
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## TODO want to be able to color PS1 iff PS1 > x chars. doable?
PS1='[\u@\h \W]\$ '

set -o emacs

# bash vars http://tldp.org/LDP/abs/html/internalvariables.html

# Aliases
alias touche='touch' # can't stop typing this. gave up. alias'd.
alias ls='ls --color=auto'
alias ll='ls -la'
alias l.='ls -d .*' # show hidden files
# alias R='R --quiet'

# TODO get rid of all these programs pls
alias mml="wine $HOME/.wine/drive_c/Program\ Files\ \(x86\)/Multimedia\ Logic/Mmlogic.exe"
#alias lc3convert="$HOME/bin/lc3tools/lc3convert"
#alias lc3sim="$HOME/bin/lc3tools/lc3sim-tk"

alias mc=moveAndcd
moveAndcd() {
  mv $@ && cd $_
}

# OPAM configuration
. /home/moonlight/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

