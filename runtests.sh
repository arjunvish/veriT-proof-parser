#!/bin/bash
for i in ./test/smt/*.smt2;
do
  echo -e "$i\n"
  ./p.sh $i
  echo -e "\n\n"
done

