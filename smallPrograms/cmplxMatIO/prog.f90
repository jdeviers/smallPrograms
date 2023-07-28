PROGRAM test
  USE mod_interfaces
  USE mod_constants
  USE mod_procedures
  USE mod_io
  implicit none

  CHARACTER(LEN=30)   :: infile = 'Rmat.dat',outfile = 'Cmat_out.dat'
  INTEGER,PARAMETER   :: D1 = 1000, D2 = 1000

  REAL,   ALLOCATABLE :: A(:,:)
  COMPLEX,ALLOCATABLE :: B(:,:)

  !ALLOCATE(A(D1,D2))
  !CALL fillRandomMat(A,'T',100.)
  !CALL printMat(A,'Real matrix A:')
  !CALL writeMatToFile(outfile, A)
  !DEALLOCATE(A)

  !CALL readMatFromFile(outfile,A)
  !CALL printMat(A,'A matrix after reading:')

  ALLOCATE(B(D1,D2))
  CALL fillRandomMat(B,'T',40.)
  !CALL printMat(B,'Complex matrix B:')
  CALL writeMatToFile(outfile,B)
  DEALLOCATE(B)

  CALL readMatFromFile(outfile,B)
  !CALL printMat(B,'B matrix after reading:')

END PROGRAM test
