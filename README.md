# PetitC

Instructions make

# Analyse syntaxique

On transforme `if (cond) s` (sans else) en `if (cond) s else {}` (branche else qui ne fait rien)

## While et For
On transforme les `while(e) s` en `for(;e;)`, on n'a donc pas de While dans l'AST

On a également décidé d'appliquer la transformation (proposée par le sujet) de `for(d;e;l)` vers `{d;for(;e;l)}`.

Dans la grammaire, `e` match une `<expr>?` mais le remplacement de `e = None` par `e = Const True` est faite par le parser pour éviter l'option dans l'AST

L'AST ne contient que des `For of expr * (expr list) * stmt`

## Déclaration de fonction

Pour avoir de meilleurs messages d'erreurs, le champ `df_loc` de l'AST retient la localisation de la **signature** de la fonction, en caputant `$loc` dans une sous-règle inlinée

```OCaml
%inline decl_fct_sig:
|	t = typ; id = IDENT; LPAR; arg = separated_list(COMMA, var); RPAR; { (t, id, arg, $loc) }
decl_fct:
|	s = decl_fct_sig; b = block { make_df s b }
```

# Typage

## Environnements, réétiquetage
Chaque fonction de typage prend en paramètre un environnement, qui associe à chaque identifiant le type d'objet qu'il
représente (fonction ou variable), son type et son étiquette. Les environnements sont persistants ce qui permet de masquer
d'anciennes associations quand on redéclare, et de revenir à ce qui était avant lorsque l'association cesse d'exister.

Le typage des blocs demande de garder aussi l'ensemble des identifiants qui ont été utilisés au sein du bloc, pour ne pas
réutiliser tout de suite un même nom.

Les étiquettes sont des informations qui seront utiles lors de la production de code :
- pour une fonction, un label de la forme `f_n_nom` où `n` est un identifiant numérique unique pour chaque fonction
	(variable `nbfuns` dans `typer.ml`), et `nom` est le nom initial de la fonction.

- pour une variable, sa position dans le tableau d'appel de la fonction dans laquelle elle est déclarée, et la profondeur d'imbrication de cette fonction.

Le calcul des positions dans le tableau d'appel est fait de telle sorte à utiliser le moins d'espace possible (quand un
bloc est terminé, on considère que l'espace qu'il utilisait est libéré et on recommence à attribuer les positions à partir
de là).
