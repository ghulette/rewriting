SOURCES = option.mli option.ml strategy.mli strategy.ml
RESULT = strategy

ANNOTATE = yes

all : byte-code-library native-code-library

include OCamlMakefile
