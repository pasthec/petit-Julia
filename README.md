# petit Julia
Projet du cours langages de programmation et compilation année 2020-2021.


Conflits du parseur (25-11-20):
Faire menhir -v parser.mly pour créer le fichier parser.conflicts qui les explique

Warning: 2 states have shift/reduce conflicts.
Warning: one state has reduce/reduce conflicts.
Warning: 32 shift/reduce conflicts were arbitrarily resolved.
Warning: 3 reduce/reduce conflicts were arbitrarily resolved.

Tests de syntaxe au 25-11-2020 (tous les tests omis, en dehors de ceux de syntax/bad, sont réussis)
Syntax/bad (devraient être refusés):
Tous les tests sont bien refusés !

Syntax/good (devraient être acceptés)
File "tests/syntax/good/testfile-block-1.jl", line 1, characters 9-10:  
syntax error
File "tests/syntax/good/testfile-block-5.jl", line 2, characters 9-10:
syntax error
File "tests/syntax/good/testfile-block-6.jl", line 2, characters 3-4:                                      
syntax error
File "tests/syntax/good/testfile-block-7.jl", line 2, characters 3-4:
syntax error
File "tests/syntax/good/testfile-semicolon-1.jl", line 1, characters 16-17:
syntax error
File "tests/syntax/good/testfile-struct-3.jl", line 2, characters 9-10:
syntax error
File "tests/syntax/good/testfile-struct-4.jl", line 2, characters 9-10:
syntax error
File "tests/syntax/good/testfile-struct-5.jl", line 2, characters 11-12:
syntax error
File "tests/syntax/good/testfile-struct-6.jl", line 2, characters 10-11:
syntax error
File "tests/syntax/good/testfile-struct-7.jl", line 2, characters 12-13:
syntax error


Typing/good (devraient être acceptés)
File "tests/typing/good/testfile-nothing-2.jl", line 2, characters 22-23:
syntax error 

Typing/bad (devraient être acceptés)
File "tests/typing/bad/testfile-duplicate-2.jl", line 2, characters 11-12:
syntax error
File "tests/typing/bad/testfile-duplicate-3.jl", line 2, characters 11-12:
syntax error
File "tests/typing/bad/testfile-scope-1.jl", line 1, characters 34-35:
syntax error
File "tests/typing/bad/testfile-scope-2.jl", line 2, characters 34-35:
syntax error

Exec (j'en fais juste quelques uns, les omis ne sont pas testés, de toute façon si on a des erreurs sur des fichiers simples peu de chances que ceux-là marchent)
abr_mut échoue
arith, arith2, assign, bool réussissent
conditional et dispatch réussissent
expo échoue :
File "tests/exec/expo.jl", line 4, characters 20-21:
syntax error

Majoritairement, on semble avoir des problèmes de séparateurs