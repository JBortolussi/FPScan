AC_INIT([tiny], 1.0, [ploc@garoche.net])

AC_PATH_PROG([OCAMLC],[ocamlc],[:])
AC_MSG_CHECKING(OCaml version)
ocamlc_version=`$OCAMLC -v | grep version | rev| cut -d \  -f 1 | rev`
major=`echo $ocamlc_version | cut -d . -f 1`
minor=`echo $ocamlc_version | cut -d . -f 2`
if (test "$major" -eq 4 -a "$minor" -eq 07 ); then
  AC_MSG_RESULT([Ocaml version is currently set as 4.07: BddApron could be used])
  AC_SUBST(BDDAPRON, "bddapron.bddapron")
  AC_SUBST(EXCLUDEMODULES, "")
  AC_SUBST(BDDAPRON_DOMAINS, "(module BddapronWrapper.PolkaBDD); (module BddapronWrapper.OctBDD);  (module BddapronWrapper.BoxBDD);")
  else		       
  AC_MSG_RESULT([Ocaml version is currently NOT set as 4.07: BddApron could NOT be used])
  AC_SUBST(BDDAPRON, "")
  AC_SUBST(EXCLUDEMODULES, "bddapronWrapper")
  AC_SUBST(BDDAPRON_DOMAINS, "")
fi
AC_MSG_RESULT(valid ocaml version detected: $ocamlc_version)

AC_PATH_PROG([OCAMLBUILD],[ocamlbuild],[:])


AC_CONFIG_FILES([lib/dune
		 lib/domains.ml
		])

AC_OUTPUT
