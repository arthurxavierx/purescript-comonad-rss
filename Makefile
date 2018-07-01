OUTDIR = public
SRC = src

SOURCES = $(wilcard $(SRC)/**/*.purs)

OUTPUT = $(OUTDIR)/index.js

all: $(OUTPUT)

deploy: $(OUTPUT)
	npx gh-pages -d $(OUTDIR)

build:
	pulp build

test:
	pulp test

node_modules/:
	npm install

bower_components/:
	bower install

$(OUTDIR)/index.js $(OUTDIR)/%.js %.js: $(SOURCES) | node_modules/ bower_components/
	pulp browserify --to $@

clean:
	rm -rf $(OUTPUT) output/

.PHONY: all $(OUTPUT) build clean test
