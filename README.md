# /!\\ OCaml 4.02.3

À cause d'une régression dans le paquet Bignum présent dans le dépôt opam :

  - Bignum marche normalement pour OCaml 4.02.3
  - pour une version d'OCaml > 4.02.3, il manque un fichier d'interface

Puisque OCaml 4.02.3 n'est plus beaucoup utilisé, nous avons fait le choix
de corriger nous-mêmes le problème en écrivant le fichier `zarith_1_4.mli`, de
telle manière que le programme fonctionne pour OCaml > 4.02.3.

Cependant, pour OCaml 4.02.3, où Bignum fonctionne normalement, ce fichier
n'aurait pas lieu d'être et cause des problèmes à la compilation (conflit sur
les interfaces). Pour compiler le projet avec OCaml 4.02.3, le plus simple est
donc de supprimer tous les \*.mli et les lignes qui correspondent dans le Makefile…


# Dépendances, compilation

`opam`, `ocaml-native-compilers`

```
opam install zarith core bignum batteries cmdliner
```

`make && make doc` produit l'exécutable "mh" et génère la documentation dans "doc"

`make clean` ou `make distclean` (supprime la doc également)


# CLI

Voir `./mh --help`.

Exemple :

```
$ ./mh genkey 10
Name: bob
Directory to write the keys in [default .]:
ou
$ ./mh genkey 10 bob .

$ ./mh showkey bob
Private key
n = 10
seq = [18 36 59 114 235 503 999 1985 3978 7982]
p = 34871
a = 27628
a_inv = 29137

$ ./mh encrypt bob.pub < in > out
ou
$ ./mh encrypt bob.pub in -o out (ou -o out in, in > out, -o out < in, etc.)
(idem avec decrypt et atk)

$ date | ./mh encrypt bob.pub
163700 123616 169920 88947 (...)

$ date | ./mh encrypt bob.pub | ./mh decrypt bob
mercredi 21 décembre 2016, 21:23:33 (UTC+0100)

$ ./mh encrypt bob.pub msg.txt | ./mh atk dynamic bob.pub
Bonjour Bob c'est Alice é_è <3

$ ./mh encrypt bob.pub msg.txt | ./mh atk tree bob.pub
Bonjour Bob c'est Alice é_è <3

$ ./mh encrypt bob.pub msg.txt | ./mh atk lll bob.pub
Bonjour Bob c'est Alice é_è <3
```
