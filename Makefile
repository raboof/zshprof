all: log2callgrind

log2callgrind: *.ml
	ocamlfind ocamlopt -linkpkg -thread -package str Callgrind.ml ZshXtrace.ml ZshXtraceToCallgrind.ml -o log2callgrind

.PHONY: clean
clean:
	rm log2callgrind *.cmi *.cmx *.o || true
