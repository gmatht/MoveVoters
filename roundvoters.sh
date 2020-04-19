ocamlc -g -I /usr/local/lib/ocaml/4.02.3/glpk         glpk.cma str.cma   -o roundvoters roundvoters.ml && # roundvoters.cmo
if [ ! -z "$1" ]
then 
	OCAMLRUNPARAM=b ./roundvoters "$@"
fi

