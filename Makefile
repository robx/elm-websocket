.PHONY: all format build examples docs

all: build

format:
	elm-format --yes src
	$(MAKE) -C examples format

build:
	elm-make

examples:
	$(MAKE) -C examples build

docs:
	elm-make --docs docs.json
