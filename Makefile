all: secdc secdrun

secdrun: machine.cmo secdrun.cmo
	ocamlc -o $@ machine.cmo secdrun.cmo

secdc: machine.cmo parser.cmi parser.cmo lexer.cmo secdc.cmo
	ocamlc -o $@ machine.cmo parser.cmo lexer.cmo secdc.cmo

%.cmi: %.mli
	ocamlc $^

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo .cmx

.mll.mli:
	ocamllex $<

.mll.ml:
	ocamllex $<

.mly.mli:
	ocamlyacc $<

.mly.ml:
	ocamlyacc $<

.mli.cmi:
	ocamlc -c $^

.ml.cmo:
	ocamlc -c $^

test: secdc secdrun
	./secdc test.source -v
	./secdrun test.bytecode -v

fact: secdc secdrun
	./secdc fact.source -v
	./secdrun fact.bytecode -v

test2: secdc secdrun
	./secdc test2.source -v
	./secdrun test2.bytecode -v

clean:
	rm -rf *.cm* secdc *~ \#*\# *.mli *.bytecode secdrun
