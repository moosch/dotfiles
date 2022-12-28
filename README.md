# Dotfiles

The obligatory dotfiles so I can jump between work and personal stuff.

## Emacs

The emacs config requires `use-package` which comes from
[Melpa](https://melpa.org/#/)

The `init.el` file will add Melpa to it's list of package repos, but `use-package` needs to be downloaded manually.

Open Emacs, and run `M-x package-refresh-contents RET`. Once that completes you should have access to Melpa packages, so running `M-x package-install RET use-package RET` will install `use-package`.
