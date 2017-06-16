SHELL = /bin/bash
.RECIPEPREFIX = >
git_repo_root ?= $(PWD)/..
local_bin_path ?= $(HOME)/.local/bin
resolver := lts-8.16

dev-all : check int-test-basic

ci-all : check


install : check
> stack --resolver $(resolver) --local-bin-path="$(local_bin_path)" $(STACK_ARGS) build --copy-bins

int-test-full :

int-test-basic :

check : build
> stack --resolver $(resolver) build --test

build :
> stack --install-ghc --resolver $(resolver) build --test --only-dependencies
> stack --resolver $(resolver) build 
