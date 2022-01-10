
*HAMIMI Dany  (hamimi sur gaufre) 21952735
Kaabeche Rayane (kaabeche sur gaufre) 21955498
L3 Informatique Groupe 3
Projet de PF5*

  


  

**

## 1) Fonctionnalités :
- read : La fonction read marche parfaitement elle prend en argument le chemin vers un fichier .p et le transforme en un block 
- reprint : Cette fonction fonctionne aussi, tout y est respecté, comme l'indentation 
-  eval : Eval fonction aussi assez bien mais a des soucis lorsque 2 arguments dans une comparaison sont des variables, comme par exemple "IF + i i < 4" que nous n'avons pas réussi à résoudre à part ce souci la fonction en général fonctionne bien.
- simpl  : simpl ici simplifie les expression et condition mais il nous manque la partie qui élimine les blocs "mort" et il initialise les read à 1 (nous avons pas réussi à juste avoir une variable sans un contenu) 
- vars : La fonction var fonctionne parfaitement, elle nous permet d'afficher toutes les variables qui ont été utilisées dans le code dans un premier temps et dans un second temps d'afficher celles qui n'ont pas été initialisées mais quand même utilisées.
-Nous n'avons pas eu le temps d'implémenter -sign par manque de temps et nous n'avons pas non plus d'extensions ou de nouvelles fonctionnalités ajouté au projet .

## 2) Compilation et exécution :
Pour compiler le projet dans un premier temps veuillez en premier faire "make" dans votre terminal.
Vous pourrez ensuite lancer les différentes fonction à l'aide des lignes de commandes suivantes :

- ./run -reprint "chemin vers votre fichier .p" -> affiche votre programme

- ./run -eval exemples/fibo.p -> execute votre programme

- ./run -simpl exemples/fibo.p -> simplifie votre porgramme

- ./run -vars exemples/fibo.p -> vous donne les variables de votre programme utilisées, et celles utilisées sans avoir été initialisées


## 3) Découpage modulaire :


## 4) Organisation du travail :
- Rayane KAABECHE : Fonction read , simpl et une partie de reprint 
- Dany HAMIMI : Fonction eval , vars et une partie de reprint

## 5) Misc :
Nous avons eu le sentiment que le projet était un peu compliqué, compte tenu de la charge de travail général (les 4 autres projets et les révisions), nous avons eu malheureusement pas assez de temps à accorder à ce projet. Mais malgré tout cela, nous avons trouvé le projet très enrichissant et nous avons eu du plaisir à le faire  
