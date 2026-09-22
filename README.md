# vectormain
Examples of Vector Functions and Subroutines in Fortran

# Compilation Methods

# Method 1: Using make
make
./vector_demo

# Method 2: Manual compilation
gfortran -c vector_module.f90          # Creates vector_module.o and vector_ops.mod
gfortran -c external_func.f90          # Creates external_func.o
gfortran -c vector_main.f90            # Creates vector_main.o
gfortran vector_module.o external_func.o vector_main.o -o vector_demo
./vector_demo

# Method 3: All at once (order matters!)
gfortran vector_module.f90 external_func.f90 vector_main.f90 -o vector_demo
./vector_demo
