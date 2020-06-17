eval "$(rbenv init -)"
# Set up rbenv for use in applications launched by Plasma, namely Emacs and the flycheck packages
# KDE seems to have their own set of environment variables, seperate from ordinary terminals
# So emacs does not have rbenv added to $PATH when launched through KDE, but it does work through terminals
