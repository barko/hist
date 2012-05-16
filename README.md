An ocaml module and command-line tool for producing histograms of
data.  Example:

    $ ./hist.native -min-value 0 -max-value 1 -num-bins 250 -i data.txt -o hist.txt

Dependencies:

  * Mikmatch
  * ocamlbuild
  * ocamlfind

Build with:

    $ ocamlbuild -use-ocamlfind hist.native
