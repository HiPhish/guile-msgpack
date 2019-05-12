# Copyright 2018 Alejandro Sanchez
#
# This file is part of msgpack-guile.
#
# Msgpack-guile is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Msgpack-guile is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with msgpack-guile.  If not, see <http://www.gnu.org/licenses/>.


# ===[ Public variables ]======================================================
# Set the following variables when invoking make:
PREFIX = ./build
GUILE = guile
TEXI2ANY = texi2any

# ---[ Private variables ]-----------------------------------------------------
# Version of Guile, used to construct library path
guile_ver = $(shell $(GUILE) -c '(display (effective-version))')

# ===[ Phony targets ]=========================================================
# These targets can be specified when invoking make
.PHONY: all lib doc html pdf clean check

all: lib doc

lib: $(PREFIX)/share/guile/site/$(guile_ver)/msgpack \
	$(PREFIX)/share/guile/site/$(guile_ver)/msgpack.scm

doc: $(PREFIX)/share/info/guile-msgpack.info

html: $(PREFIX)/html/guile-msgpack.html

pdf: $(PREFIX)/pdf/guile-msgpack.pdf

clean:
	@rm -rf $(PREFIX)
	@rm -f *.log  # Log files from testing

check: ./test
	@for f in test/*.scm;        do guile -L . $$f; done
	@for f in test/pack/*.scm;   do guile -L . $$f; done
	@for f in test/unpack/*.scm; do guile -L . $$f; done


# ===[ File targets ]==========================================================
# Targets which actually build files, used by the phony targets
$(PREFIX)/share/guile/site/$(guile_ver)/msgpack: msgpack
	@mkdir -p $(dir $@)
	@cp -r $< $@

$(PREFIX)/share/guile/site/$(guile_ver)/%.scm: %.scm
	@mkdir -p $(dir $@)
	@cp -r $< $@

$(PREFIX)/share/info/%.info: doc/%.texi
	@mkdir -p $(dir $@)
	@$(TEXI2ANY) --info -I $(dir $<)/include --output=$@ $<

$(PREFIX)/html/%.html: doc/%.texi
	@mkdir -p $(dir $@)
	@$(TEXI2ANY) --html --no-split --no-headers -I $(dir $<)/include --output=$@ $<

$(PREFIX)/pdf/%.pdf: doc/%.texi
	@mkdir -p $(dir $@)
	@texi2pdf --quiet --clean -I $(dir $<)/include --output=$@ $<
