# A collection of simple FORTRAN utilities

To compile, run:
```
make clean
make
```
with the Makefile in the working directory. Tested on Ubuntu 18.04, 20.04 and 22.04 with the gfortran compiler. 

---

 - ## CmplxMatIO
   Can read from, and write to, an ASCII file a 2D array. Can also create a random-, complex-valued 2D array.
   Finally, can write 2D arrays to screen with user-friendly formatting. Any aforementioned array can be either real, complex, or integer. 
   Reading, writing (*i.e.* to file) and printing (*i.e.* to screen) subroutines are interfaced in order to permit the use of a generic procedure.

   This code was originally intended as a way of generating complex-valued, variously-shaped complex matrices mimicking the form of a spin-density matrix, for use with other FORTRAN programs to compute a [singlet yield](https://doi.org/10.1080/00268979809483134). More generally, it was an exercise in handling I/O with complex types, and in interfacing procedures. This code can still serve as a platform for implementing more matrix operations on square or rectangular arrays.

   **To compile:** requires only make and gfortran.
   
   **To run:** simply run `./test` (the name of the executable -- can be modified to something more meaningful in the makefile).

   **Notes on usage:** When performing I/O operations, **in**- and **out**-files (*i.e.* files to be **read** and **written to**, respectively) are specified as parameter strings, in the `prog.f90` file. As of now, only one infile and one outfile can be handled.

   **Input:**
    - (Optional) infile (CHARACTER(LEN=30)) (inside `prog.f90`): filename of ASCII file containing REAL or COMPLEX-valued 2D array.
    - (Optional) outfile (CHARACTER(LEN=30)) (inside `prog.f90`): filename of ASCII file to which write an array.
    - (Optional) D1,D2 (INTEGER) (inside `prog.f90`): extents of dimensions 1 and 2 of a random-valued, REAL or COMPLEX 2D array.

   **Output:**
    - 2D arrays, REAL or COMPLEX, written to screen or to file.

   **Future dev:** Future developments to the code, if any, could include providing the infile as an argument, and also implementing a way of generating outfile filenames via substring substitution.
 ---
   
 - ## dipolarCoupling
   Can compute the electron-electron dipolar coupling [(EED)](https://arxiv.org/pdf/1806.01519.pdf) (in the point-dipole approximation), between 2 electrons *a* and *b* separated by a vector $r_{ab}$.
   This code was intended to quickly evaluate the strength of the EED coupling as a function of distance. Within this context, the vectorial nature of $r_{ab}$ is not relevant, as only its norm is required. However, all routines in the `mod_procedures` module can be readily used in, for instance, the setup of a spin Hamiltonian and the propagation of a spin density matrix (such a code, although in Matlab, was used [there](https://doi.org/10.1063/5.0078115)).

   **To compile:** Requires make, gfortran and a LAPACK installation. The location of the library must be set in the Makefile.
   
   **To run:** run `./dipolar`. No arguments can be provided.

   **Notes on usage:** As a standalone application, only the norm of the interactomic vector is needed. If possible, provide it such that the r_ab is of the form `(0,0,norm)` to avoid issues with the subsequent diagonalisation of A.
   
   **Input:**
    - r_ab (REAL(dp),DIMENSION(3)) (inside `prog_dipole.f90`): inter-electron vector.

   **Output:**
    - d (REAL(dp)) (to screen): norm of the EED coupling.
    - A (REAL(dp),DIMENSION(3,3)) (to screen): EED coupling matrix.
    - W (REAL(dp),DIMENSION(3)) (to screen): diagonalised EED coupling matrix.
  
   **Future dev:** If the `mod_procedures` module is to be included into a wider program, it should be able to also diagonalise nonsymmetric matrices. A strategy as that implemented in the **MatrixDiag** project could be applied here.
   
 - ## generalPurposeXyzIoAndManips
   Can read, write, and modify an [.xyz](https://open-babel.readthedocs.io/en/latest/FileFormats/XYZ_cartesian_coordinates_format.html) format file (for atomic coordinates). Average quantities can be computed on atomic coordinates, such as the center of mass ([COM](https://en.wikipedia.org/wiki/Center_of_mass#In_three_dimensions)) or the Root-Mean-Square Deviation ([RMSD](https://en.wikipedia.org/wiki/Root-mean-square_deviation_of_atomic_positions)) with respect to a reference geometry. Both these quantities can be computed using either a function or a subroutine. The program can also read and write REAL 2D arrays, and perform substring substitution -- again, either using a subroutine of a function.
   The initial purpose of this code was simply an exercise in implementing the same algorithm as a subroutine and as a function.

   **To compile:** requires only make and gfortran.

   **To run:** run `./caller.x`. No arguments can be provided.

   **Notes on usage:**

   **Input:**
    - (Optional) infile (CHARACTER(LEN=*)) (inside `prog_caller.f90`): filename of .xyz file.
    - (Optional) outfile (CHARACTER(LEN=*)) (inside `prog_caller.f90`): filename of ASCII file to which write an array.

   **Output:**
    - .xyz arrays written to screen or to file.
    - 2D arrays, REAL or COMPLEX, written to screen or to file.
    - RMSD (REAL): Root-Mean-Square Deviation of atomic positions.
    - COM (REAL,DIMENSION(3)): Center of Mass.

   **Future dev:** Implementation of the ability to read a .xyz *trajectory* file. Compute the [RMSF](https://en.wikipedia.org/wiki/Mean_squared_displacement).

--- 

 - ## MatrixDiag
   Diagonalises a NxN, real-valued matrix, symmetric or not. The matrix is provided, in free-format, inside a separate file; the path to which is provided at runtime as an argument to the program. It interfaces LAPACK's double precision matrix diagonalisation routines [DSYEV](https://netlib.org/lapack/explore-html/d2/d8a/group__double_s_yeigen_ga442c43fca5493590f8f26cf42fed4044.html) (for symmetric matrices) and [DGEEV](https://www.netlib.org/lapack/explore-html/d9/d8e/group__double_g_eeigen_ga66e19253344358f5dee1e60502b9e96f.html) (nonsymmetric). These routines consist in an efficient implementation of the [QR algorithm](https://en.wikipedia.org/wiki/QR_algorithm) for generating eigenvalues and eigenvectors to a matrix. This code was initially intended to allow a quick and handy evaluation of eigenvalues for [hyperfine coupling tensors](https://en.wikipedia.org/wiki/Hyperfine_structure). It was also included in a bash pipeline for computing averaged magnetic properties, with downstream operation on eigenvalues performed in simple python script, before the whole thing was turned into a single python script.

   **To compile:** Requires make, gfortran and a LAPACK installation. The location of the library must be set in the Makefile.

   **To run:** run `./diag_nxn <input_file>`. An example input matrix file is provided: see `N5.dat`.

   **Notes on usage:** The input matrix must be given in a separated file and not fed to the executable as a stream. Its element do not have to follow a fixed formatting: they can be given as floats (with any number of significant digits), or as integers (which will be read as double precision floats anyway). Elements of the same row must be separated by at least one space or tab. A new row is declared by inserting a newline.

   **Input:**
    - (string, as argument) input matrix (REAL(dp),DIMENSION(N,N)): input square matrix. N is the extent of the matrix in either dimension.

   **Output:**
    - SYMMETRIC MATRIX:
      - W (REAL(dp),DIMENSION(N)): eigenvalues of the diagonalised input matrix.
      - A (REAL(dp),DIMENSION(N,N)): eigenvectors of the diagonalised input matrix. 
    - NONSYMMETRIC MATRIX:
      - WR (REAL(dp),DIMENSION(N)): REAL part of the eigenvalues of the diagonalised input matrix.
      - WI (REAL(dp),DIMENSION(N)): IMAGINARY part of the eigenvalues of the diagonalised input matrix.
      - VL (REAL(dp),DIMENSION(N,N)): LEFT eigenvectors of the diagonalised input matrix.
      - VR (REAL(dp),DIMENSION(N,N)): RIGHT eigenvectors of the diagonalised input matrix.
     
   **Future dev:** N/A. 

---

 - ## MatrixRotation
   Linearly transforms (*i.e.* rotates and scales) an input matrix A by applying a [transformation matrix](https://en.wikipedia.org/wiki/Transformation_matrix) B.
   
   **To compile:** requires only make and gfortran.

   **To run:** run `./rotmat <input_matrix_path> <rotation_matrix_path>`.

   **Notes on usage:** The input matrices must be given in a separated file and not fed to the executable as a stream. Their elements do not have to follow a fixed formatting: they can be given as floats (with any number of significant digits), or as integers (which will be read as double precision floats anyway). Elements of the same row must be separated by at least one space or tab. A new row is declared by inserting a newline. The input matrix and the rotation matrix must have the same shape.

   **Input:**
   	- (string, as argument) input matrix (REAL(dp),DIMENSION(N,N)): input square matrix. N is the extent of the matrix in either dimension.
   	- (string, as argument) rotation matrix (REAL(dp),DIMENSION(N,N)): input square matrix.

   **Output:**
   	- (to screen) A (REAL(dp),DIMENSION(N,N)): input matrix.
   	- (to screen) B (REAL(dp),DIMENSION(N,N)): transformation matrix.
   	- (to screen AND to file) C (REAL(dp),DIMENSION(N,N)): rotated input matrix.

   **Future dev:** N/A.

---
