export CFLAGS="-march=native -O2 -pipe"
export CXXFLAGS="${CFLAGS}"
myconfigure (){
	./configure && nice make -j2
}
alias sudo=`which sudo` # If sudo is not installed, assume root user
alias apt-get="apt-get -y"
command -v ocamlopt || sudo apt-get install ocaml-nox
command -v git || sudo apt-get install git-core
command -v ocamlfind || sudo apt-get install ocaml-findlib
command -v lzip || sudo apt-get install lzip
export LD_LIBRARY_PATH=/usr/local/lib

[ -e gmp-6.0.0a.tar.lz ] || wget https://gmplib.org/download/gmp/gmp-6.0.0a.tar.lz
tar -xf gmp-6.0.0a.tar.lz 
[ -e /usr/local/lib/libgmp.a ] || ( cd gmp-6.0.0 && myconfigure && sudo make install ) 

[ -e glpk-4.47.tar.gz ] || ( wget ftp://ftp.gnu.org/gnu/glpk/glpk-4.47.tar.gz  # [13356] [RETRN_VAL=0]
	tar -zxf glpk-4.47.tar.gz  # [13356] [RETRN_VAL=0]
)
[ -e /usr/lib/libglpk.a ] ||( cd glpk-4.47/ && ./configure --with-gmp && nice make -j2 && sudo make install )

ln -s /usr/local/lib/libglpk* /usr/lib/

[ -e ocaml-glpk ] || git clone https://github.com/smimram/ocaml-glpk.git # [10622] [RETRN_VAL=0]
(cd ocaml-glpk
	git checkout  src/glpk_stubs.c
	git checkout d8ba110161347f19dc173d112c501fd656b7616d
	sed -i.bak s/lpx_simplex/lpx_exact/ src/glpk_stubs.c
	make clean	
	nice make 
	sudo make install
)
make
bash ./install.sh

