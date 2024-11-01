# PetitC

## État du projet

- Toutes les parties du compilateur sont fonctionnelles, et supportent les fonctions imbriquées.
- Résultats des tests : 217/217 à la syntaxe, 172/172 au typage, 81/81 à la production de code.
- Les messages d'erreur de typage sont les plus précis possibles (localisation et nature du problème).

## Instructions

Les modules OCaml nécessaires sont `menhir` et `ppx_deriving` (plus précisement le plugin `show`) pour auto-générer les pretty-printers avec `[@@deriving show {with_path = false}]`. Voir https://github.com/ocaml-ppx/ppx_deriving

- Le Makefile appelle `dune build` et `dune clean`.
- La règle `make autotest` lance le script `tests/autotest.sh` en mode 2b par défaut.
- Pour choisir le mode, il suffit d'écraser la variable F, par exemple `make autotest F="1b"` pour tester l'analyse syntaxique.

`./petitc` est un lien symbolique vers l'exécutable `_build/default/petitc.exe`.

En plus de `--debug-alloc`, deux autres options ont été rajoutées, `--print-ast` et `--print-typed-ast`.

# Analyse syntaxique

## If

On transforme `if (cond) s` (sans else) en `if (cond) s else {}` (branche else qui ne fait rien).

## While et For
On transforme les `while(e) s` en `for(;e;)`, on n'a donc pas de While dans l'AST.

On a également décidé d'appliquer la transformation (proposée par le sujet) de `for(d;e;l)` vers `{d;for(;e;l)}`.

Dans la grammaire, `e` match une `<expr>?` mais le remplacement de `e = None` par `e = Const True` est faite par le parser pour éviter l'option dans l'AST.

L'AST ne contient que des `For of expr * (expr list) * stmt`.

## Déclaration de fonction

Pour avoir de meilleurs messages d'erreurs, le champ `df_loc` de l'AST retient la localisation de la **signature** de la fonction, en caputant `$loc` dans une sous-règle inlinée.

```OCaml
%inline decl_fct_sig:
|	t = typ; id = IDENT; LPAR; arg = separated_list(COMMA, var); RPAR; { (t, id, arg, $loc) }
decl_fct:
|	s = decl_fct_sig; b = block { make_df s b }
```

La règle `var` du parser qui lit type + identifiant, utilisée pour les paramètres et les variables, retient la localisation (`Ast.var = typ * string * loc`) afin d'avoir un message précis en cas de redéfinition de paramètre dans une signature (si `int f(int x, int x)`, le typeur renverra une erreur sur les caractères 14-19 et pas sur la signature entière).

## Directives `#include`
On vérifie que les `#include` n'ont comme arguments que les trois fichiers, `stdbool.h`, `stdlib.h` et `stdio.h`, et on
rejette n'importe quel autre `#include`.

# Typage

## Environnements, réétiquetage
Chaque fonction de typage prend en paramètre un environnement, qui associe à chaque identifiant le type d'objet qu'il
représente (fonction ou variable), son type et son étiquette. Les environnements sont persistants ce qui permet de masquer
d'anciennes associations quand on redéclare, et de revenir à ce qui était avant lorsque l'association cesse d'exister.

Initialement, nous avions un environnement pour les variables et un pour les fonctions, mais nous avons regroupé les deux
avec un environnement sur un type somme quand nous nous sommes rendu compte qu'une variable pouvait cacher une fonction
(ou vice-versa) et qu'on ne saurait pas déterminer qui masque qui autrement.

Les étiquettes sont des informations qui seront utiles lors de la production de code :
- pour une fonction, une étiquette de la forme `f_n_nom` où `n` est un identifiant numérique unique pour chaque fonction attribué dans l'ordre d'apparition dans le programme (variable `nbfuns` dans `typer.ml`), et `nom` est le nom initial de la fonction.
On conserve aussi sa profondeur d'imbrication.

- pour une variable, sa position dans le tableau d'appel de la fonction dans laquelle elle est déclarée, et la profondeur d'imbrication de cette fonction. Le nom de la variable est oublié car il ne nous sera plus utile par la suite.

## Programme typé
La fonction `type_prog` renvoie l'AST typé, utile pour le debug. Elle génère aussi une liste `funs`, qui contient toutes
les déclarations de fonctions du programme (y compris celles imbriquées), triées par ordre d'apparition : c'est celle là
qui nous intéressera lors de la production de code.

## Le cas spécial `type_block`

### Arguments persistants, variables mutables

La fonction de typage des blocs est l'unique endroit du typeur à utiliser des références comme **variables locales**, initialisées par des arguments qui eux, par contre, sont persistants

```OCaml
and type_block ?(be_init = Sset.empty) env_init expect in_loop fpover fun_depth raw_block =
  let env = ref env_init in
  let block_env = ref be_init in
  let fpcur = ref fpover and max_prefix_fp = ref 0 in
  (* type_decl : decl -> t_decl *)
  let block_ty = List.map type_decl raw_block in
  block_ty, (max !fpcur !max_prefix_fp)
```

En effet, la persistance est utile uniquement quand on rentre dans un sous-bloc et il y a dans ce cas persistance au moment du passage des arguments : le sous-bloc "verra" de nouvelles références.

Cela évite de devoir faire rentrer et ressortir les environnements dans `type_decl`. Ainsi `type_decl` est du type `decl -> t_decl` au lieu de `tenv -> Sset.t -> decl -> tenv * Sset.t * t_decl` et elle modifie les références du bloc courant. Cela a grandement allégé le code.

### `block_env`

En plus d'un environnement classique, on a `block_env` qui est l'ensemble (mutable) des identifiants qui ne peuvent pas être réutilisés : les variables du bloc courant. Les sous-blocs ne le modifient pas car ils "voient" une autre référence.

Si le bloc est un corps de fonction la fonction `type_fct` passe un argument optionnel `be_init` contenant les noms des paramètres.

### Ajout dans l'environnement

Quand on déclare une variable, on l'ajoute à l'environnement **après** le typage de l'expression qui l'initialise (pour éviter `int x = x;`, même si c'est autorisé en C).

Quand on déclare une fonction, on l'ajoute à l'environnement **avant** de la typer pour qu'elle puisse être récursive.

# Production de code

Notre code compile toutes les fonctions du programme dans l'ordre de la liste `funs` produite par le typeur, et non en
suivant l'AST typé (qui est quand même renvoyé par le typeur à des fins de debug). Les déclarations imbriquées de fonctions
sont donc ignorées, et toutes les fonctions sont compilées comme des fonctions globales.

## Registres utilisés
Pour limiter le nombre d'opérations sur la pile, le résultat d'une expression se trouve dans `%rax`. C'est aussi le registre
utilisé pour les valeurs de retour de fonction.
Le registre `%rbx` ne sert qu'à stocker le deuxième opérande des opérations binaires.
Le registre `%rdx` ne sert que pour les divisions euclidiennes, car il est utilisé par l'opération assembleur `idivq`.

## `compile_lvalue`

Cette fonction sert à récupérer, dans `%rax`, l'adresse mémoire d'une expression qui est une valeur gauche, sans accéder 
directement à la valeur de l'expression. Ceci sert pour les opérateurs `&`, `++`, `--` et `=`.

## Utilisation des étiquettes

Trois structures créent des étiquettes :
- les opérateurs binaires `&&` et `||` créent une étiquette, utilisée pour ignorer l'évaluation du deuxième opérande si le
résultat est connu après celle du premier. Il y a une symétrie entre les deux cas, ce qui permet de les traîter en même temps.

- les conditions créent deux étiquettes, une pour ignorer le bloc `if` et la deuxième pour ignorer le bloc `else`.

- les boucles créent quatre étiquettes
	- une première au début du corps de la boucle
	- une deuxième entre le corps et les expressions calculées à chaque pas : utilisée par `continue`
	- une troisième entre ces expressions et la condition : on y saute au début, puis un saut conditionnel est fait vers le
	premier label, permettant une structure plus linéaire avec moins de sauts
	- un quatrième label à la fin : utilisé par `break`

Chaque étiquette contient, entre autres, un identifiant numérique unique, attribués dans l'ordre de rencontre lors de la
compilation.

## `rewind_rbp`

Puisque les profondeurs d'imbrication sont connues depuis le typage, il suffit d'écrire le bon nombre de fois une instruction
qui remonte au `%rbp` du parent. `rewind_rbp n` remonte de `n` étages, et le `%rbp` atteint est stocké dans `%rax`.
