default: help

help:
	@echo "make all"

all: emacs brew tmux zsh

emacs:
	ln -s $(PWD)/.emacs.d ~/

brew:
	ln -s $(PWD)/.brewfile ~/

tmux:
	ln -s $(PWD)/.tmux.conf ~/

zsh:
	ln -s $(PWD)/.zshrc ~/
