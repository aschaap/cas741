# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

INFRA := prelude.cmo
OBJ   := rkgen.cmo
TEST  := tests

VERSION := 1.0

########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: test $(TEST)

.PHONY: test 
test: $(OBJ) $(TEST)
	./tests -runner sequential -shards 1

.PHONY: clean
clean:
	rm -f $(TEST) *.cm[io]

$(OBJ): $(INFRA)

########################################################################
#
# Implicit Rules
#
########################################################################

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	metaocamlc -w Ax -g -c $<

########################################################################
#
# Explicit Rules
#
########################################################################



tests: rkgen.ml
	OCAMLFIND_COMMANDS="ocamlc=metaocamlc" ocamlfind ocamlc -w Ax -g -o $@ -package oUnit -linkpkg -g prelude.ml rkgen.ml test-rkgen.ml
