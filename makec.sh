#!/bin/bash

str=$1
n=$2

int_file="out/$str/$str.int"
jac_file="out/$str/$str.jac"

mkdir -p "out/$str"
sh expand.sh $str > $int_file
sh jacobian.sh $str $n > $jac_file
python makec.py $int_file $jac_file "out/$str" $n

