# Project Name Source Code

The folders and files for this project are as follows:

rkgen.ml - the core file containing the generator
prelude.ml - supplementary file with some basic type definitions
test-rkgen.ml - contains unit tests and system tests
Makefile - creates test executable from test-rkgen.ml and runs it immediately

When installing OCaml, it is important to choose a method that provides OPAM, the package manager for OCaml. This is needed for MetaOCaml.
On Linux, this would entail using the distribution's package manager; for Ubuntu, it would be a simple as `sudo apt-get install ocaml opam`.
On macOS, Homebrew seems to be a good way to get OPAM: `brew install ocaml` and `brew install opam` should do the trick.
On Windows, there are a few options, see https://ocaml.org/docs/install.html#Windows

Through OPAM, one can install MetaOCaml by running `opam switch 4.02.1+BER` for example for BER MetaOCaml v4.02.1.
For more details on MetaOCaml, see http://okmij.org/ftp/ML/MetaOCaml.html#install