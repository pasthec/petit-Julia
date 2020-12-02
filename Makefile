pjuliac: ast.ml dune dune-project lexer.mll parser.mly pjulia.ml printer.ml typer.ml
	dune build
	cp _build/default/pjulia.exe pjuliac
	chmod +w pjuliac

clean:
	rm -r _build
