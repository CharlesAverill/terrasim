.PHONY: default build install uninstall test clean fmt
.IGNORE: fmt

OPAM ?= opam
OPAM_EXEC ?= $(OPAM) exec --
DUNE ?= dune
PYTHON ?= python

default: build

fmt:
	$(OPAM_EXEC) $(DUNE) fmt
	$(OPAM_EXEC) $(DUNE) promote

SPRITE_DIR := assets/sprites
SPRITE_OUTPUT := lib/assets/sprites.ml
SPRITE_SCRIPT := assets/sprites/gen_sprites_file.py

FONT_DIR := assets/fonts
FONT_OUTPUT := lib/assets/fonts.ml
FONT_SCRIPT := assets/fonts/gen_fonts_file.py

$(SPRITE_OUTPUT): $(SPRITE_SCRIPT) $(wildcard $(SPRITE_DIR)/*)
	$(PYTHON) $(SPRITE_SCRIPT) $(SPRITE_OUTPUT)

$(FONT_OUTPUT): $(FONT_SCRIPT) $(wildcard $(FONT_DIR)/*)
	$(PYTHON) $(FONT_SCRIPT) $(FONT_OUTPUT)

build: fmt $(SPRITE_OUTPUT) $(FONT_OUTPUT)
	$(OPAM_EXEC) $(DUNE) build --profile=release

install:
	$(OPAM_EXEC) $(DUNE) install

uninstall:
	$(OPAM_EXEC) $(DUNE) uninstall

clean:
	$(OPAM_EXEC) $(DUNE) clean
	git clean -dfXq

profile: build
	perf record --call-graph=dwarf -- ./_build/default/bin/main.exe

profile_report:
	perf report

profile_vis:
	perf script | stackcollapse-perf.pl > out.perf-folded
	flamegraph.pl out.perf-folded > perf.svg
	firefox perf.svg

test: fmt
	$(OPAM_EXEC) $(DUNE) runtest

testf: fmt
	$(OPAM_EXEC) $(DUNE) runtest -f

run: build
	$(OPAM_EXEC) $(DUNE) exec -- TerraSim

debug: build
	$(OPAM_EXEC) ocamldebug _build/terrasim/TerraSim/main.bc

DOCS_PATH=docs/
DOCS_NAME=TerraSim
DOCS_DESCR=A SimEarth clone
DOCS_INDEX_TITLE=$(DOCS_NAME) - $(DOCS_DESCR)
define DOCS_EMBED
<meta content="$(DOCS_NAME)" property="og:title" />\
<meta content="$(DOCS_DESCR)" property="og:description" />\
<meta content="https://github.com/CharlesAverill/TerraSim" property="og:url" />
endef

cleandocs:
	if [ ! -d $(DOCS_PATH) ]; then \
		mkdir $(DOCS_PATH); \
	fi
	rm -rf $(DOCS_PATH)module $(DOCS_PATH)docs $(DOCS_PATH)odoc.support $(DOCS_PATH)index.html

docs: cleandocs build
	$(OPAM_EXEC) $(DUNE) build @doc
	mv -f _build/terrasim/_doc/_html/* $(DOCS_PATH)
	rm -f $(DOCS_PATH)index.html
	mv $(DOCS_PATH)TerraSim/TerraSim.html $(DOCS_PATH)index.html
	mv $(DOCS_PATH)TerraSim $(DOCS_PATH)module
	
	@echo "Preparing Index\n--------------"
	# Header
	sed -i 's/<title>.*<\/title>/<title>$(DOCS_INDEX_TITLE)<\/title>/g' $(DOCS_PATH)index.html
	sed -i 's@</head>@$(DOCS_EMBED)\n</head>@g' $(DOCS_PATH)index.html
	sed -i 's/..\/odoc.support/odoc.support/g' $(DOCS_PATH)index.html
	# Body
	sed -i "s@<nav class="odoc-nav">.*gbcamel</nav>@@g" $(DOCS_PATH)index.html

push: cleandocs build
	@read -p "Commit message: " input; \
	if [ -z "$input" ]; then \
		echo "Error: Please provide a valid commit message."; \
		exit 1; \
	fi; \
	git add . && git commit -m "$$input" && git push origin main
