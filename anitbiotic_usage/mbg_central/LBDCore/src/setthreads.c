/*
C functions that we make use of in R for dynamically setting OMP & MKL threads

To compile and create the shared library, do the following:
 * Make sure that the MKL is installed somewhere (in this example it is in
   /opt/intel)
 * Set the following environmental variables (this is done automatically in the
   Singularity image):
   export LD_LIBRARY_PATH=/opt/intel/compilers_and_libraries_2018.0.128/linux/tbb/lib/intel64_lin/gcc4.7:/opt/intel/compilers_and_libraries_2018.0.128/linux/compiler/lib/intel64_lin:/opt/intel/compilers_and_libraries_2018.0.128/linux/mkl/lib/intel64_lin
   export CPATH=/opt/intel/compilers_and_libraries_2018.0.128/linux/mkl/include
   export NLSPATH=/opt/intel/compilers_and_libraries_2018.0.128/linux/mkl/lib/intel64_lin/locale/%l_%t/%N
   export LIBRARY_PATH=/opt/intel/compilers_and_libraries_2018.0.128/linux/tbb/lib/intel64_lin/gcc4.7:/opt/intel/compilers_and_libraries_2018.0.128/linux/compiler/lib/intel64_lin:/opt/intel/compilers_and_libraries_2018.0.128/linux/mkl/lib/intel64_lin
   export MKLROOT=/opt/intel/compilers_and_libraries_2018.0.128/linux/mkl
 * Compile and create shared library with:
   gcc -std=gnu99 -DMKL_ILP64 -fopenmp -m64 -I${MKLROOT}/include -I/usr/lib/gcc/x86_64-linux-gnu/7.3.0/include -I/usr/local/R-3.5.0/include -DNDEBUG -fpic -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c setthreads.c -o setthreads.o
   g++ -shared -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl -o setthreads.so setthreads.o

Make sure that the three includes in the compile step to the object file (-c)
actually point to a directory containing the appropriate header file. In the
above, we have:
 * -I${MKLROOT}/include                          : "mkl.h"
 * -I/usr/lib/gcc/x86_64-linux-gnu/7.3.0/include : "omp.h"
 * -I/usr/local/R-3.5.0/include                  : "R.h"
From within R, you can do `R.home('include')` which will show where the header
files live

Compile and link options were obtained by using Intel's Link Line Advisor:
https://software.intel.com/en-us/articles/intel-mkl-link-line-advisor/
and also taking a simple example (hello.c from "Statistical Computing in C++ and
R" by Eubank and Kupresanin, page 186-187, available free online from CRC) and
seeing what options were used when doing `R CMD SHLIB hello.c`
(see https://stash.ihme.washington.edu/users/imdavis/repos/testing/browse/simple_C_in_R/hello.c)

The additional -lR flag might be necessary when linking to a shared library if
linking against an libR.so, as well as including the -L/usr/lib/R/lib (or
wherever the libR.so lives) which we do not have in the LBD Singularity image,
ex:
g++ -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl -o setthreads.so setthreads.o -lR

After the setthreads.so shared library is built (in /opt/compiled_code_for_R
directory for example), you can load in the shared library to use the functions
in R by doing:
dyn.load("/opt/compiled_code_for_R/setthreads.so")

You can test that the function has been loaded by doing (in R). The following
should all return TRUE
  is.loaded("setMKLthreads")
  is.loaded("setOMPthreads")
  is.loaded("setMKLdynamic")
  is.loaded("setOMPnested")

Execute the C functions in R by doing:
  .C("setMKLthreads", as.integer(1))
  .C("setOMPthreads", as.integer(1))

Normally, we have OMP_NESTED enabled and MKL_DYNAMIC disabled as suggested by
Intel here: https://software.intel.com/en-us/articles/recommended-settings-for-calling-intel-mkl-routines-from-multi-threaded-applications
  .C("setOMPnested", as.integer(1))  # enables
  .C("setMKLdynamic", as.integer(0)) # disables
Which we can now do within R as well.

Setting MKL threads is discussed here: https://software.intel.com/en-us/mkl-macos-developer-guide-techniques-to-set-the-number-of-threads

As discussed in "Statistical Computing in C++ and R" by Eubank and Kupresanin,
page 186-187 (free PDF's are available online), the following should be followed
when using the ".C" interface between R and C:
 * There should be no main function
 * Have void as a return type
 * Have arguments that are pointers
Rprintf function is the R analog to printf in C brought in through the R.h headers

This code along with some testing is under version control here:
https://stash.ihme.washington.edu/users/imdavis/repos/testing/browse/mclapply_mkl
*/

#include <R.h>
#include <mkl.h>
#include <omp.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

// Function to set MKL threads (supersedes MKL_NUM_THREADS)
void setMKLthreads(int *threads)
{
  Rprintf("Setting MKL threads to %d\n", *threads);
  mkl_set_num_threads(*threads);
}

// Function to set OMP threads (supersedes OMP_NUM_THREADS)
void setOMPthreads(int *threads)
{
  Rprintf("Setting OMP threads to %d\n", *threads);
  omp_set_num_threads(*threads);
}

// Function to enable/disable Intel MKL to dynamically change the number of
// OpenMP threads
// == 0 disables
// != 0 enables
void setMKLdynamic(int *val)
{
  if(*val == 0) {
    Rprintf("MKL dynamic OpenMP adjustment DISABLED\n");
  } else {
    Rprintf("MKL dynamic OpenMP adjustment ENABLED\n");
  }
  mkl_set_dynamic(*val);
}

// Function to enable/disable nested OpenMP parallelism
// == 0 disables
// != 0 enables
void setOMPnested(int *val)
{
  if(*val == 0) {
    Rprintf("OpenMP nested parallelism DISABLED\n");
  } else {
    Rprintf("OpenMP nested parallelism ENABLED\n");
  }
  omp_set_nested(*val);
}
