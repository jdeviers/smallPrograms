PROGRAM quick_diag
!
! Diagonalises real, square matrices (symm or not; checks itself).
! Submit matrix as a file (filename as arg).
! Do not submit matrix as a stream: any argument is interpreted as a filename.
! Returns eigenvalues and eigenvectors.
!
  use mod_precision
  use mod_procedures
  use mod_diag
  implicit none

  REAL(dp),ALLOCATABLE  :: A(:,:)
  INTEGER(i4)           :: i,j
  LOGICAL               :: SYM
  INTEGER               :: N

  CALL COUNT_ROWS(1,N)
  ALLOCATE(A(N,N))

  CALL READ_NxN_MAT(1,N,A)

! Print input matrix:
  WRITE(*,'(A)') 'Input matrix:'
  CALL PRINTSQMAT(A,N)
!
  WRITE(*,'(/,A)') 'Current specs: NxN real matrix.'
  SYM = CHECKSYMM(A,N)

! For nonsymmetric, real matrix
  IF (.NOT. SYM) THEN
    CALL DIAG_ASYM_NXN(N,A)
  END IF

! For symmetric, real matrix:
  IF (SYM) THEN
    CALL DIAG_SYM_NXN(N,A)
  END IF

  DEALLOCATE(A)
END PROGRAM quick_diag

