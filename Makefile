pjuliac: ast.ml dune dune-project lexer.mll parser.mly pjulia.ml printer.ml typer.ml x86_64.ml compile.ml
	dune build
	cp _build/default/pjulia.exe pjuliac
	chmod +w pjuliac

clean:
	rm -rf _build
	rm pjuliac
