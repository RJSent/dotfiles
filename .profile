export XDG_CONFIG_HOME=~/.config

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)" # Rbenv setup

export IRBRC="$XDG_CONFIG_HOME"/irb/irbrc # FIXME: default to ~/.config if $XDG.. is unset instead of setting $XDG
