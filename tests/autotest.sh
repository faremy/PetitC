#!/bin/bash

shopt -s nullglob

# script de test pour le projet de compilation

option=$1
compilo=$2
score=0
max=0
bonus=0
verbose=0

echo "Test de $2"

echo

# tous les tests passent avec gcc
test_gcc() {
for f in syntax/bad/*.c; do
    if gcc -c $f > /dev/null 2>&1 ; then
      echo "succès de gcc -c sur $f"
    fi
done

for f in typing/good/*.c exec/*.c exec-fail/*.c; do
    gcc -c $f > /dev/null 2>&1 ||
     echo "echec de gcc sur $f"
done
for f in typing/bad/*.c; do
    if gcc -c $f > /dev/null 2>&1 ; then
      echo "succès de gcc -c sur $f"
    fi
done
for f in exec/*.c; do
    echo "test gcc sur $f"
    expected=exec/`basename $f .c`.out
    if gcc $f > /dev/null 2>&1 ; then
      ./a.out > out
      if ! cmp --quiet out $expected; then
          echo "mauvaise sortie de gcc sur $f"
      fi
    else
      echo "échec de gcc -c sur $f"
    fi
done
}

compile () {
if [[ $verbose != 0 ]]; then
  echo Compile $1 $2
  $compilo $1 $2;
else
  $compilo $1 $2 > /dev/null 2>&1;
fi;
}


# partie 1 : tests d'analyse syntaxique

partie1 () {

score=0
max=0

echo "Partie 1"

# les mauvais
echo -n "mauvais "
for f in syntax/bad/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait échouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in syntax/good/*.c typing/bad/*.c typing/good/*.c exec/*.c exec-fail/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Syntaxe : $score/$max : $percent%"; }

# partie 2 : tests d'analyse sémantique


partie2 () {
echo
echo "Partie 2"

score=0
max=0

# les mauvais
echo -n "mauvais "
for f in typing/bad/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait échouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in typing/good/*.c exec/*.c exec-fail/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo    "Typage  : $score/$max : $percent%";
}


# partie 3 : tests d'exécution

partie3 () {

score_comp=0
score_out=0
score_test=0
max=0

echo
echo "Partie 3"
echo "Execution normale"
echo "-----------------"

# timeout="why3-cpulimit 30 0 -h"
# spim="spim -ldata 20000000 -lstack 20000000"

for f in exec/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n "."
    asm=exec/`basename $f .c`.s
    rm -f $asm
    expected=exec/`basename $f .c`.out
    max=`expr $max + 1`;
    if compile $f; then
	rm -f out
	score_comp=`expr $score_comp + 1`;
	if gcc -no-pie $asm && ./a.out > out; then
	    score_out=`expr $score_out + 1`;
	    if cmp --quiet out $expected; then
		score_test=`expr $score_test + 1`;
	    else
		echo
		echo "ECHEC : mauvaise sortie pour $f"
	    fi
	else
		echo
		echo "ECHEC du code produit pour $f"
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait réussir)"
    fi
done
echo

echo "Execution conduisant à un échec"
echo "-------------------------------"

for f in exec-fail/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n "."
    asm=exec-fail/`basename $f .c`.s
    rm -f $asm
    max=`expr $max + 1`;
    if compile $f && gcc -no-pie $asm; then
	score_comp=`expr $score_comp + 1`;
	if ./a.out > out; then
	    echo
	    echo "ECHEC : devrait échouer sur $f"
	else
	    score_test=`expr $score_test + 1`;
	    score_out=`expr $score_out + 1`;
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait réussir)"
    fi
done

echo
percent=`expr 100 \* $score / $max`;

echo "Compilation:";
percent=`expr 100 \* $score_comp / $max`;
echo "Compilation : $score_comp/$max : $percent%";
percent=`expr 100 \* $score_out / $max`;
echo "Code produit : $score_out/$max : $percent%";
percent=`expr 100 \* $score_test / $max`;
echo "Comportement du code : $score_test/$max : $percent%";}


case $option in
    "-1" )
        partie1;;
    "-2" )
        partie2;;
    "-3" )
        partie3;;
    "-1b" )
	bonus=1;
	partie1;;
    "-2b" )
    	bonus=1;
        partie2;;
    "-3b" )
    	bonus=1;
        partie3;;
    "-all" )
    	partie1;
    	partie2;
    	partie3;;
    "-allb" )
        bonus=1;
    	partie1;
    	partie2;
    	partie3;;
    "-gcc" )
        test_gcc;;
    * )
        echo "usage : $0 <option> <compilo>"
        echo "spécifier une option parmi : "
        echo "-1      : tester la partie 1"
        echo "-2      : tester la partie 2"
        echo "-3      : tester la partie 3"
        echo "-all    : tout tester"
        echo "-1b     : tester la partie 1 avec fonctions imbriquées"
        echo "-2b     : tester la partie 2 avec fonctions imbriquées"
        echo "-3b     : tester la partie 3 avec fonctions imbriquées"
        echo "-allb   : tout tester avec fonctions imbriquées";;

esac
echo
