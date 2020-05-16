#!/bin/bash
SMTFILE=$1
echo -e "Building parser...\n"
make
echo -e "\n"
cvc4 --dump-proofs --no-lfsc-letification $SMTFILE > result
if [[ $(cat result | head -n 1) == "unsat" ]]; then
  echo -e "SMT problem is unsat. Getting proof...\n"
  cat result | tail -n +2 > proof.plf
  rm result
  if [ $? -eq 0 ]; then
    echo -e "Proof in proof.plf. Checking proof...\n"
    lfscc sigs/sat.plf sigs/smt.plf sigs/th_base.plf sigs/th_bv.plf sigs/th_bv_bitblast.plf sigs/th_arrays.plf sigs/th_bv_rewrites.plf proof.plf
    echo -e "\nChecking whether assertions in proof correspond to assertions in SMT file...\n"
    ./main.native proof.plf $SMTFILE
    rm proof.plf
  else
    echo -e "Couldn't generate proof!\n"
  fi
else
  echo "SMT problem is not unsat."
fi
