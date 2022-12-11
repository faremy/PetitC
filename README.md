# PetitC

- Rémy KIMBROUGH
- Hugo PEYRAUD-MAGNIN

## État du projet

- Notre typeur est fonctionnel avec nested (172/172).
- Les messages d'erreur de typage sont les plus précis possibles (localisation et nature du problème).
- Nous avons aussi anticipé "au maximum" la production de code (le typeur s'occupe notamment de calculer les offset des variables), ce que vous pouvez constater avec l'option `--debug-alloc`.

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
- pour une fonction, un label de la forme `f_n_nom` où `n` est un identifiant numérique unique pour chaque fonction attribué
dans l'ordre d'apparition dans le programme (variable `nbfuns` dans `typer.ml`), et `nom` est le nom initial de la fonction.
On conserve aussi sa profondeur d'imbrication (utile uniquement pour le debug).

- pour une variable, sa position dans le tableau d'appel de la fonction dans laquelle elle est déclarée, et la profondeur d'imbrication de cette fonction. Le nom de la variable est oublié car il ne nous sera plus utile par la suite.

## Programme typé
La fonction `type_prog` renvoie l'AST typé, utile pour le debug. Elle génère aussi une liste `funs`, qui contient toutes
les déclarations de fonctions du programme (y compris celles imbriquées), triées par ordre d'apparition : c'est celle là
qui nous intéressera lors de la production de code.

## Le cas spécial type_block

### Arguments persistents, variables mutables

La fonction de typage des blocs est l'unique endroit du typeur à utiliser des références comme **variables locales**, initialisées par des arguments qui eux, par contre, sont persistents

```OCaml
and type_block ?(be_init = Sset.empty) env_init expect in_loop fpover fun_depth raw_block =
  let env = ref env_init in
  let block_env = ref be_init in
  let fpcur = ref fpover and max_prefix_fp = ref 0 in
  (* type_decl : decl -> t_decl *)
  let block_ty = List.map type_decl raw_block in
  block_ty, (max !fpcur !max_prefix_fp)
```

En effet, la persistence est utile uniquement quand on rentre dans un sous-bloc et il y a dans ce cas persistance au moment du passage des arguments : le sous-bloc "verra" de nouvelles références.

Cela évite de devoir faire rentrer et ressortir les environnements dans `type_decl`. Ainsi `type_decl` est du type `decl -> t_decl` au lieu de `tenv -> Sset.t -> decl -> tenv * Sset.t * t_decl` et elle modifie les références du bloc courant. Cela a grandement allégé le code.

### block_env

En plus d'un environnement classique, on a `block_env` qui est l'ensemble (mutable) des identifiants qui ne peuvent pas être shadow : les variables du bloc courant. Les sous-blocs ne le modifient pas car ils "voient" une autre référence.

Si le bloc est un corps de fonction la fonction `type_fct` passe un argument optionnel `be_init` contenant les noms des paramètres.

### Ajout dans l'environnement

Quand on déclare une variable, on l'ajoute à l'environnement **après** le typage de l'expression qui l'initialise (pour éviter `int x = x;`, même si c'est autorisé en C).

Quand on déclare une fonction, on l'ajoute à l'environnement **avant** de la typer pour qu'elle puisse être récursive.
