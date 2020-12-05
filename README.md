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
