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

Initialement, nous avions un environnement pour les variables et un pour les fonctions, mais nous avons regroupé les deux
avec un environnement sur un type somme quand nous nous sommes rendu compte qu'une variable pouvait cacher une fonction
(ou vice-versa) et qu'on ne saurait pas déterminer qui masque qui autrement.

Le typage des blocs demande de garder aussi l'ensemble des identifiants qui ont été utilisés au sein du bloc, pour ne pas
réutiliser tout de suite un même nom.

Les étiquettes sont des informations qui seront utiles lors de la production de code :
- pour une fonction, un label de la forme `f_n_nom` où `n` est un identifiant numérique unique pour chaque fonction
(variable `nbfuns` dans `typer.ml`), et `nom` est le nom initial de la fonction. On conserve aussi sa profondeur
d'imbrication (utile uniquement pour le debug).

- pour une variable, sa position dans le tableau d'appel de la fonction dans laquelle elle est déclarée, et la profondeur d'imbrication de cette fonction.
