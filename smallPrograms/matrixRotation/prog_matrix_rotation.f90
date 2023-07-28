PROGRAM matrix_rotation
!
! Takes 2 NxN matrices as arguments: 
!   * the first is the matrix to be rotated
!   * the second is the transformation matrix
! Output matrix written to 'rotated.dat' and to screen.
!
  use mod_precision
  use mod_procedures
  implicit none

  REAL(dp),ALLOCATABLE  :: A(:,:),B(:,:),C(:,:)
  INTEGER(i4)           :: i,j
  LOGICAL               :: SYM
  INTEGER               :: N

  CALL COUNT_ROWS(1,N)
  ALLOCATE(A(N,N),B(N,N),C(N,N))

  CALL READ_NxN_MAT(1,N,A)
! Print input matrix 1:
  WRITE(*,'(A)') 'Matrix to be rotated:'
  CALL PRINTSQMAT(A,N)

  CALL READ_NxN_MAT(2,N,B)
! Print input matrix 2:
  WRITE(*,'(A)') 'Transformation matrix:'
  CALL PRINTSQMAT(B,N)

! C = B x A x transpose(B)
  C = matmul(B,matmul(A,transpose(B)))
  
! Print output matrix:
  WRITE(*,'(A)') 'Rotated matrix:'
  CALL PRINTSQMAT(C,N)

! Write output matrix to file 'rotated.dat'
  WRITE(*,'(A)') 'Writing to file:' 
  CALL PRINT_TOFILE_SQMAT(C,N,'rotated.dat')

  DEALLOCATE(A,B,C)
END PROGRAM matrix_rotation

