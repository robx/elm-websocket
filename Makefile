.PHONY: all format build

all: build

format:
	elm-format --yes src

build:
	elm-make
