# petit Julia
Projet du cours langages de programmation et compilation année 2020-2021.

La compilation se fait en exécutant `make` dans le dossier principal, qui fait appel à `dune` ( penser à faire `eval $(opam env)` si il y a des erreurs liées à `opam` ) puis déplace l'exécutable `pjuliac` dans le dossier principal et le rend exécutable.

La commande `make clean` permet d'enlever tous les fichiers créés par la compilation.

Le programme s'utilise en faisant `./pjuliac` suivi éventuellement des arguments suivants :
```
--parse-only   stop after parsing
  --type-only  stop after typing
  --pretty-print  print the ast produced after parsing
  -help  Display this list of options
  --help  Display this list of options
```

Le pretty printer permet d'afficher une représentation de l'ast obtenu après le parsing sous la forme d'un code petit-julia ( avec quelques différences comme les accolades pour les `if...else` pour délimiter leur portée ) pour pouvoir débugger ou éventuellement désoffusquer un code.


Tests au 16/1 matin : 

Tests de exec qui passent : arith, arith2,assign, bool, dispatch2, fact_it, fact_it2, fact_rec, fact_rec2, fib, for, for2, for3, for4, for5, hello_world, hello_world2, int64, lazy, many, mutual, quine, scope1, scope2, scope3, scope4, while
Erreur sur : 

julia et mandelbrot : mauvais résultat

queens : 
tests/exec/queens.s: Assembler messages:
tests/exec/queens.s:1432: Error: symbol `instr_17' is already defined
