#usr/bin/bash
#Afficher un tableau du temps de calcul pour chaque exemples.
list_ex="chencousot_sas2015 intloopcounter intloopcounter2 b1 bool1 boolean ex01 randombool simplest test_eq trex01"

domain=$1
domainbdd="$1bdd"

declare -a name=()
declare -a time=()
declare -a timebdd=()
declare -a loopinv=()
declare -a loopinvbdd=()

for element in $list_ex
do
    name[${#name[*]}]="$element"
    res=$(time (tiny -a $domain -v 10 examples/ex_boolpart/$element.tiny) 2>&1)
    time[${#time[*]}]=$(echo "$res" | grep "user" | cut -d'r' -f2)
    loopinv[${#loopinv[*]}]=$(echo "$res" | grep "loop invariant" -A10 | tr '\n' ' ' | cut -d':' -f2- | cut -d'/' -f1)
    res2=$(time (tiny -a $domainbdd -v 10 examples/ex_boolpart/$element.tiny) 2>&1)
    timebdd[${#timebdd[*]}]=$(echo "$res2" | grep "user" | cut -d'r' -f2)
    loopinvbdd[${#loopinvbdd[*]}]=$(echo "$res2" | grep "loop invariant" -A10 | tr '\n' ' ' | cut -d'}' -f1 | cut -d":" -f2)
done
echo tableau des temps de calcul
echo "\begin{center}"
echo "\begin{tabular}{ | l | c | c | }"
echo "\hline"
echo "\multicolumn{3}{|c|}{Time,s}\\\\"
echo "\hline"
echo "& $domain Bddapron & Boolpartition x $domain Apron\\\\"
echo "\hline"
for ((i=0; i<11;i++))
do
    echo "${name[$i]} & ${timebdd[$i]} & ${time[$i]}\\\\"
    echo "\hline"
done
echo "\end{tabular}"
echo "\end{center}"
echo
echo

#tableau des invariant de boucle pour les exemples qui en ont qui ne fonctionne pas très bien 
#echo tableau des invariant de boucle
#echo "\begin{center}"
#echo "\begin{tabular}{ | l | c | c | }"
#echo "\hline"
#echo "\multicolumn{3}{|c|}{loop invariant}\\\\"
#echo "\hline"
#echo "& $domain Bddapron & Boolpartition x $domain Apron\\\\"
#echo "\hline"
#for ((i=0; i<11;i++))
#do
#    echo "${name[$i]} & ${loopinvbdd[$i]}} & ${loopinv[$i]}}\\\\"
#    echo "\hline"
#done
#echo "\end{tabular}"
#echo "\end{center}"
