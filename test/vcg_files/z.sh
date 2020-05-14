#!/bin/bash
for i in $(find -name *.smt2)
do
  wc -l $i
done
