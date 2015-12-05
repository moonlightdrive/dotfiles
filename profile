# environment vars only pls
export EDITOR="emacsclient --alternate-editor= -t"
export BROWSER="firefox"

export EC2_HOME=/usr/local/ec2/ec2-api-tools-1.7.1.2
export JAVA_HOME=/usr/lib/jvm/default
export R_LIB=$HOME/.r/library
export PATH=$PATH:$HOME/.cabal/bin:$EC2_HOME/bin:$HOME/bin/lc3tools:$HOME/.npm:/usr/local/heroku/bin

#export JAVA_HOME=/usr/lib/jvm/default-runtime/bin/java


# https://github.com/the-lambda-church/merlin/wiki/Letting-merlin-locate-go-to-stuff-in-.opam
export OPAMKEEPBUILDDIR=true
export OCAMLPARAM="_,bin-annot=1"

# private env vars
PDOTS=$HOME/.config/priv_dots
[[ -f $PDOTS/private_profile ]] && . $PDOTS/private_profile
