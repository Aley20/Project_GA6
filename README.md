Groupe AS

Pour compiler le projet :
$ make

Pour exécuter le programme (version basique) :
$ ./latsi_basique tests/basique/test1.txt

Pour exécuter le programme (version extension 1) :
$ ./latsi_ex1 tests/ex1/test1.txt

Pour exécuter le programme (version extension 2) :
$ ./latsi_ex2 tests/ex2/test1.txt

Pour exécuter le programme (version extension 3) :
$ ./latsi_ex3 tests/ex3/test1.txt


Le projet a été parfaitement réaliser par Aleysa AKBAS et Leiticia RAAB. 
Nous avons fait le projet jusqu'à l'étape 4 inclue, ainsi que les 3 extensions : 

— Ajouter la possibilité d’avoir plusieurs affectations qui se suivent sur la même ligne : X=1,Y=2+4. Ici les affectations sont évaluées comme si elles étaient sur deux lignes qui se suivent.
— Ajouter la possibilité d’avoir des instructions de la forme X,Y,Z = 1,2+5,4 avec un nombre quelconque de variables à gauche du = et le même nombre d’expressions à droite. Ici, les expressions à droite sont évaluées simultanément. Donc si X vaut 2 et on fait X,Y = 5,X+1, Y vaudra 3 après. X,Y = 2 déclenche une erreur.
— Ajouter une instruction SOUSROUTINE <expression> (qui saute vers la ligne donnée par l’expression) et une instruction RETOURNE (qui revient à la première ligne non vide après la dernière instruction SOUSROUTINE exécuté).

