# Dotfiles

## General Info
This is a repository of personal dotfiles that I use on my system. For anyone
unaware, dotfiles refer to files that are used by programs to store various settings,
like font size in a terminal. The term comes from how files names can prefaced by a . (dot),
which in Linux (and I assume all Unix-like systems) is used to hide the file. This
is common for customization files, as most people do not want to see them all the time.
I am using something called a git bare repository in order to track my dotfiles.

You'll find configurations for common things like bash and Emacs,
as well as some less frequently customized, like IRB (the Ruby interpreter).

Some customizations are done for the obvious reason of customizing my programs, 
others are done so that the customizations work in the first place! :)

Over time this should expand and grow. Who knows, maybe a few
config files will actually look good in the process.

## Acknowledgements
As nothing is 100% original nowadays, I used these two articles as
a guide for how to set this up. (Who in turn found out from other sources. I doubt one person
can be credited for this idea.) Between the two articles it should be achievable for
anyone familiar with a terminal to track dotfiles using this method.

[Atlassian](https://www.atlassian.com/git/tutorials/dotfiles) and [Harfang](https://harfangk.github.io/2016/09/18/manage-dotfiles-with-a-git-bare-repository.html).

## Warnings
I make no guarantee that any or every file in this repository is in a working state.

Additionally, you should NOT just clone my repository to your machine. First, there may
be a recursion issue mentioned in the articles above. But more importantly, many of
these dotfiles are customized to my use! My workflow is likely different than yours. Instead,
I would suggest you pick and choose what customizations you would like to use. I try to comment most
nontrivial sections so it's clear what exactly the goal behind it is.