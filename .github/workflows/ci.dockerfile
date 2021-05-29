ARG tag=alpine
ARG image=ocaml/opam:$tag
FROM $image
MAINTAINER Nicolas “Niols” Jeannerod

RUN opam install dune

ADD . .
RUN sudo chown -R opam .

RUN opam exec -- dune build 2>/dev/null || true
## FIXME: The caught failure of dune build is here to mitigate #4487.

RUN opam pin . --no-action
RUN for pkg in $(opam pin list --short); do \
      opam depext $pkg --with-doc --with-test; \
      opam install $pkg --deps-only --with-doc --with-test; \
    done

RUN opam exec -- make
RUN opam exec -- make test
RUN opam exec -- make doc
RUN opam exec -- make install
RUN opam exec -- make uninstall
RUN opam exec -- make clean
