MODULE mod_diag
  use mod_precision
  use mod_procedures
  implicit none

  contains

  SUBROUTINE DIAG_SYM_NXN(N,A)
    implicit none

    REAL(dp),ALLOCATABLE  :: A(:,:)
    INTEGER               :: N,LDA
    INTEGER,PARAMETER     :: LWMAX = 1000
    INTEGER               :: INFO,LWORK
    REAL(dp),ALLOCATABLE  :: W(:)
    REAL(dp)              :: WORK(LWMAX)

    LDA = N
    ALLOCATE(W(N))
! Find optimal internal settings for DSYEV  
    LWORK = -1
    CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
    LWORK = MIN(LWMAX,INT(WORK(1)))
! Solve eigenproblem
    CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
! Check for convergence.
    IF (INFO.GT.0) THEN
      WRITE(*,*)'The algorithm failed to compute eigenvalues.'
      STOP
    END IF
! Eigenvalues print
    WRITE(*,'(/,A)') 'Eigenvalues (in ascending order):'
    CALL PRINTVEC(W,N)
    CALL PRINT_TOFILE_VEC(W,N,'eigenvalues.dat')
! Eigenvectors print
    WRITE(*,'(/,A)') 'Orthonormal eigenvectors:'
    CALL PRINTSQMAT(A,N)
    CALL PRINT_TOFILE_SQMAT(A,N,'eigenvectors.dat')

    DEALLOCATE(W)
  END SUBROUTINE DIAG_SYM_NXN

  SUBROUTINE DIAG_ASYM_NXN(N,A)
    implicit none

    REAL(dp),ALLOCATABLE  :: A(:,:)
    INTEGER,PARAMETER     :: LWMAX = 1000
    INTEGER               :: N,LDA,LDVL,LDVR
    INTEGER               :: INFO,LWORK
    REAL(dp)              :: WORK(LWMAX)
    REAL(dp),ALLOCATABLE  :: VL(:,:),VR(:,:),WR(:),WI(:)

    LDA = N; LDVL = N; LDVR = N
    ALLOCATE(VL(LDVL,N), VR(LDVL,N), WR(N), WI(N))
! Find optimal internal settings for DGEEV
    LWORK = -1
    CALL DGEEV('Vectors','Vectors',N,A,LDA,WR,WI,VL,LDVL,VR,LDVR,WORK,LWORK,INFO)
    LWORK = MIN(LWMAX,INT(WORK(1)))
! Solve eigenproblem.
    CALL DGEEV('Vectors','Vectors',N,A,LDA,WR,WI,VL,LDVL,VR,LDVR,WORK,LWORK,INFO)
! Check for convergence.
    IF (INFO.GT.0) THEN
      WRITE(*,*)'The algorithm failed to compute eigenvalues.'
      STOP
    END IF
! Eigenvalues print
    WRITE(*,'(/,A)') 'REAL part of the eigenvalues:'
    CALL PRINTVEC(WR,N)
    CALL PRINT_TOFILE_VEC(WR,N,'eigenvalues_realpart.dat')
 
    WRITE(*,'(/,A)') 'IMAGINARY part of the eigenvalues:'
    CALL PRINTVEC(WI,N)
    CALL PRINT_TOFILE_VEC(WI,N,'eigenvalues_imaginarypart.dat')
! Eigenvectors print
    WRITE(*,'(/,A)') 'LEFT eigenvectors:'
    CALL PRINTSQMAT(VL,N)
    CALL PRINT_TOFILE_SQMAT(VL,N,'eigenvectors_left.dat')

    WRITE(*,'(/,A)') 'RIGHT eigenvectors:'
    CALL PRINTSQMAT(VR,N)
    CALL PRINT_TOFILE_SQMAT(VR,N,'eigenvectors_right.dat')

    DEALLOCATE(VL,VR,WR,WI)
  END SUBROUTINE DIAG_ASYM_NXN

END MODULE mod_diag