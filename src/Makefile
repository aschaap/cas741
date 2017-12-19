# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

INFRA := prelude.cmo
OBJ   := rkgen.cmo
TEST  := tests rkgen.out
RES   := rkgen.res

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
	diff -B -b rkgen.out rkgen.res

mytop: 
	metaocamlmktop -o mytop nums.cma $(BASE)

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

%.out: test-%.ml %.cmo mytop
	./mytop $(BASE) ./$*.cmo < $< > $@

########################################################################
#
# Explicit Rules
#
########################################################################



tests: rkgen.ml test-rkgen.ml
	OCAMLFIND_COMMANDS="ocamlc=metaocamlc" ocamlfind ocamlc -w Ax -g -o $@ -package oUnit -linkpkg -g prelude.ml rkgen.ml test-rkgen.ml

rkgen.out: test-rkgen.ml rkgen.cmo mytop
	./mytop rkgen.cmo < $< > $@

# Dependencies. Needed! Otherwise, old versions of files are compiled.

rkgen.cmo :  prelude.cmo
