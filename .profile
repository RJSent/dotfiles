# Force apps into XDG compliance
export XDG_CONFIG_HOME=~/.config
export XDG_DATA_HOME=~/.local/share

# Editor function
export EDITOR="$(command -v emacs) -nw"

# Rbenv setup
export PATH="$HOME/.rbenv/bin:$PATH"

export IRBRC="$XDG_CONFIG_HOME"/irb/irbrc # FIXME: default to ~/.config if $XDG.. is unset instead of setting $XDG

export PAGER=bat
