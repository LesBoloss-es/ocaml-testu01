ARG tag=alpine
ARG image=ocaml/opam:$tag
FROM $image
MAINTAINER Nicolas “Niols” Jeannerod

RUN opam install dune

ADD . .
RUN sudo chown -R opam .

RUN opam exec -- make
RUN opam exec -- make test
RUN opam exec -- make doc
RUN opam exec -- make install
RUN opam exec -- make uninstall
RUN opam exec -- make clean
