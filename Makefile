default: help

.PHONY: help
help:
	@echo "make all"

.PHONY: all
all: emacs brew tmux zsh

.PHONY: emacs
emacs:
	ln -s $(PWD)/emacs.d ~/.config/emacs
	ln -s $(PWD)/doom.d ~/.config/doom

.PHONY: brew
brew:
	ln -s $(PWD)/.brewfile ~/

.PHONY: tmux
tmux:
	ln -sf $(PWD)/tmux ~/.config/tmux

.PHONY: zsh
zsh:
	ln -s $(PWD)/.zshrc ~/

.PHONY: config
config:
	ln -sf $(PWD)/yabai $(HOME)/.config/yabai
	ln -sf $(PWD)/skhd $(HOME)/.config/skhd
	ln -sf $(PWD)/alacritty $(HOME)/.config/alacritty
