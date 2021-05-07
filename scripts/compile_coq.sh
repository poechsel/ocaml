#!/bin/sh -e

help() {
  echo "./compile-coq [compiler path] [path to directory]"
  echo "[compiler path] is the path to the ocaml compiler"
  echo "[path to directory] is the path to the drectory where eveything will be build." 
  echo "                    This script considers that it is free to do anything from"
  echo "                    this directory, including deleting it."
  echo "                    Omitting this entry will result in using a temp dir."
}

if [ "$#" -le 0 ]
then
  help
  exit 0
elif [ "$#" -le 1 ]
then
  echo "1"
  COMPILER_TO_TEST=$(realpath $1)
  CWD=$(mktemp -d)
else
  echo "2"
  COMPILER_TO_TEST=$(realpath $1)
  CWD=$(realpath $2)
fi
echo $COMPILER_TO_TEST
echo $CWD

mkdir -p $CWD

OCAML_PATH=$CWD/ocaml
OPAM_PATH=$CWD/opam
OPAM_INSTALL=$OPAM_PATH/_install
OPAM_ROOT=$OPAM_PATH/.opam
OPAM_EXE=$OPAM_INSTALL/bin/opam

echo $OCAML_PATH
echo $OPAM_PATH
echo $OPAM_INSTALL
echo $OPAM_ROOT
echo $OPAM_EXE


install_compiler () {
  rm -rdf $OCAML_PATH
  cp -r $COMPILER_TO_TEST $OCAML_PATH
}

install_and_build_opam () {
  git clone https://github.com/ocaml/opam $OPAM_PATH
  pushd $OPAM_PATH

  ./configure --prefix=$OPAM_INSTALL
  make lib-ext
  make
  make install
  popd

  rm -rdf $OPAM_ROOT
  $OPAM_EXE init --root=$OPAM_ROOT -n --disable-sandboxing --bare
}


install_compiler
install_and_build_opam

pushd $OCAML_PATH
$OPAM_EXE switch --root=$OPAM_ROOT create . --empty --repositories=default,beta=git+https://github.com/ocaml/ocaml-beta-repository.git
$OPAM_EXE install --root=$OPAM_ROOT  . --inplace-build -y
$OPAM_EXE pin add --root=$OPAM_ROOT  coq git+https://github.com/ocaml-flambda/coq.git#flambda2-patches -y

