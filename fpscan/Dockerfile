# Build a docker image
# with tiny excutable located at /tiny/bin/tiny
            

FROM alpine:3.12 AS builder

RUN apk add --no-cache make m4 gcc ocaml ocaml-compiler-libs ocaml-ocamldoc opam ocaml-findlib perl libc-dev mpfr-dev gmp-dev

# Configuration de OPAM
RUN opam init --auto-setup -y --disable-sandboxing && \
    eval $(opam config env)

# Copie des sources
COPY . /var/tiny

WORKDIR /var/tiny

RUN eval $(opam config env) && \ 
    opam pin -y -n tiny . && \
    opam install -y --destdir /tiny tiny && \
    opam clean

FROM alpine:3.12

# Apron libraries
COPY --from=builder /root/.opam/default/share/apron/lib/*.so /usr/local/lib/
# MPFR library
COPY --from=builder /usr/lib/libmpfr.so.6 /usr/lib/
# GMP library
COPY --from=builder /usr/lib/libgmp.so.10 /usr/lib/
# Tiny executable
COPY --from=builder /tiny /tiny