# Get R include and library paths
rlib := $(shell Rscript -e "R.home('lib')" | sed 's/\[1\] //g')
rinc := $(shell Rscript -e "R.home('include')" | sed 's/\[1\] //g')

# all: mbg_tmb_model.so setthreads.so
all: setthreads.so

mbg_tmb_model.so: mbg_tmb_model.cpp
	echo "Building MBG TMB Model"
	Rscript -e "TMB::compile('mbg_tmb_model.cpp')"

setthreads.so: setthreads.c
	echo "Compiling threading functions"
	gcc -std=gnu99 -DMKL_ILP64 -fopenmp -m64 -I$(MKLROOT)/include -I$(rinc) -DNDEBUG -fpic -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c setthreads.c -o setthreads.o
	echo "Link to LBDCore.so (only setthreads, not the TMB model)"
	g++ -shared -L$(rlib) -Wl,-Bsymbolic-functions -Wl,-z,relro -L$(MKLROOT)/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl -o LBDCore.so setthreads.o -lR
	g++ -shared -L$(rlib) -Wl,-Bsymbolic-functions -Wl,-z,relro -L$(MKLROOT)/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl -o setthreads.so setthreads.o -lR

clean:
	rm -rf *.o *.so *.gcda
