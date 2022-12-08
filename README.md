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
