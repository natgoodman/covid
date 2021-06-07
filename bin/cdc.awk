#!/bin/awk
# remember to pre-process with: sed 's/".*"//'
# cat input/cdc/cdc.21-05-24.csv | tail -n +2 | sed 's/".*"//' | awk -f bin/cdc.awk > data/cdc/cdc.21-05-24.csv

# convert Yes/No to 1/0
function yn(x) {
    x=substr(x,1,1); if (x=="Y") return(1); else if (x=="N") return(0); else return("")
}
# convert status (Laboratory-confirmed, Probable) to 1/0
function st(x) {
    x=substr(x,1,1); if (x=="L") return(1); else if (x=="P") return(0); else return("")
}
# convert age group to 1 digit (starting age / 10)
function char1(x) {
    x=substr(x,1,1); return(x); 
}
BEGIN {FS=",";OFS=","}
{print $1,char1($7),char1($6),st($5),yn($9),yn($10),yn($11)}
