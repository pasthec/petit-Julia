# petit Julia
Projet du cours langages de programmation et compilation année 2020-2021.

Tests de typage :
Les tests de typing/bad sont tous refusés.

Les tests de typing/god et exec-fail sont tous acceptés

Les tests de exec sont acceptés sauf :

File "tests/exec/dispatch2.jl", line 7, characters 9-19:

Error: Ambiguous call to f


Si une fonction est strictement plus spécifique que les autres, on doit la choisir. Donc, si jamais il y en a une parmi les fonctions compatibles, on n'échoue pas car on n'a une chance de ne pas avoir d'ambiguité.

Comparaison de spécificité entre deux fonctions implémentée. Ajouter la recherche d'une telle fonction parmi celles obtenues -> relou