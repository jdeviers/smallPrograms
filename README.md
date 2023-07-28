# A collection of simple FORTRAN utilities

To compile, run:
```
make clean
make
```
with the Makefile in the working directory. Tested on Ubuntu 18.04, 20.04 and 22.04 with the gfortran compiler. 

 - ## CmplxMatIO
   Can read from, and write to, an ASCII file a 2D array. Can also create a random-, complex-valued 2D array.
   Finally, can write 2D arrays to screen with user-friendly formatting. Any aforementioned array can be either real, complex, or integer. 
   Reading, writing (*i.e.* to file) and printing (*i.e.* to screen) subroutines are interfaced in order to permit the use of a generic procedure.

   This code was originally intended as a way of generating complex-valued, variously-shaped complex matrices mimicking the form of a spin-density matrix, for use with other FORTRAN programs to compute a [singlet yield](https://doi.org/10.1080/00268979809483134). More generally, it was an exercise in handling I/O with complex types, and in interfacing procedures. This code can still serve as a platform for implementing more matrix operations on square or rectangular arrays.

   **To compile:** requires only gfortran and make.
   **To run:** simply run `./test` (the name of the executable -- can be modified to something more meaningful in the makefile).

   **Notes on usage:** When performing I/O operations, **in**- and **out**-files (*i.e.* files to be **read** and **written to**, respectively) are specified as parameter strings, in the `prog.f90` file. As of now, only one infile and one outfile can be handled. Future develepments to the code, if any, could include providing the infile as an argument, and also implementing a way of generating outfile filenames via substring substitution.
   
 - ## dipolarCoupling
 - ## generalPurposeXyzIoAndManips
 - ## MatrixDiag
 - ## MatrixRotation
