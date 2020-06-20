# Force apps into XDG compliance
export XDG_CONFIG_HOME=~/.config
export XDG_DATA_HOME=~/.local/share

# Rbenv setup
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)" # Rbenv setup

export IRBRC="$XDG_CONFIG_HOME"/irb/irbrc # FIXME: default to ~/.config if $XDG.. is unset instead of setting $XDG
