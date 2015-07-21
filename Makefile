SOURCES = option.mli option.ml \
          term.mli term.ml \
          strategy.mli strategy.ml \
          examples.ml
RESULT = strategy

ANNOTATE = yes

all : byte-code-library native-code-library

include OCamlMakefile
