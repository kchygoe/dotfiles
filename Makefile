default: help

help:
	@echo "make all"

all: emacs brew tmux zsh

emacs:
	ln -s $(PWD)/emacs.d ~/.emacs.d
	ln -s ~/src/github.com/kchygoe/dotfiles/doom.d ~/.doom.d

brew:
	ln -s $(PWD)/.brewfile ~/

tmux:
	ln -s $(PWD)/.tmux.conf ~/

zsh:
	ln -s $(PWD)/.zshrc ~/

configdir:
	mkdir -p $(HOME)/.config/yabai
	mkdir -p $(HOME)/.config/skhd
	mkdir -p $(HOME)/.config/alacritty

config:
	ln -s $(PWD)/yabairc $(HOME)/.config/yabai/yabairc
	ln -s $(PWD)/yabairc $(HOME)/.config/skhd/skhdrc
	ln -s $(PWD)/yabairc $(HOME)/.config/alacritty/alacritty.yml
