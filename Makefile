SOURCES = option.mli option.ml \
          strategy.mli strategy.ml \
          traverse.mli traverse.ml
RESULT = strategy

ANNOTATE = yes

all : byte-code-library native-code-library

include OCamlMakefile
