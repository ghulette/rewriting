SOURCES = option.mli option.ml \
          strategy.mli strategy.ml \
          traverse.mli traverse.ml \
          example.ml
RESULT = rewrite

ANNOTATE = yes

all : byte-code-library native-code-library

include OCamlMakefile
