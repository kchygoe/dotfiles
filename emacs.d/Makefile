default: help

help:
	@echo "makefile for emacs env"

golang:
	go get -u golang.org/x/tools/cmd/goimports
	go get -u github.com/rogpeppe/godef
	go get -u golang.org/x/lint/golint
	go get -u github.com/sourcegraph/go-langserver

link:
	ln -s $(PWD) ~/

all: golang link
