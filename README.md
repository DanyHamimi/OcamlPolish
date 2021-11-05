Projet PF5 2021 : Polish
========================

## Modalités de rendu et d'évaluation

Voir [CONSIGNES.md](CONSIGNES.md)

## Prérequis à installer

Voir [INSTALL.md](https://gaufre.informatique.univ-paris-diderot.fr/letouzey/pf5/blob/master/INSTALL.md)

  - ocaml évidemment
  - dune et make

## Compilation et lancement

Par défaut, `make` est seulement utilisé pour abréger les commandes `dune` (voir `Makefile` pour plus de détails):

  - `make` sans argument lancera la compilation `dune` de `polish.exe`,
    c'est-à-dire votre programme en code natif.

  - `make clean` pour effacer le répertoire provisoire `_build` 
    produit par `dune` lors de ses compilations.

Enfin pour lancer votre programme: `./run arg1 arg2 ...`

  
