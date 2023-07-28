PROGRAM dipolar_coupling
  use mod_precision
  use mod_procedures
  implicit none

  REAL(dp),DIMENSION(3) :: r_ab = (/ 0.0,0.0,24.97/)
  REAL(dp)              :: A(3,3)
  INTEGER(i4)           :: i,j
! DSYEV parameters: DO NOT SPECIFY A KIND FOR INTEGERS OR IT CRASHES
  INTEGER,PARAMETER     :: N = 3,LDA = N,LWMAX = 1000
  INTEGER               :: INFO,LWORK
  REAL(dp)              :: W(N),WORK(LWMAX)

  CALL point_dipole_dipole_coupling(r_ab,A)
  DO i=1,3 
    WRITE(*,'(3F8.2)') (A(i,j), j=1,3)
  END DO

! Find optimal internal settings for DSYEV  
  LWORK = -1
  CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
  LWORK = MIN(LWMAX,INT(WORK(1)))
! Run DSYEV: diagonalise A
! Eigenvalues are XX,YY,ZZ values of the dipolar coupling at distance r_ab
  CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
  WRITE(*,'(A)') 'The eigenvectors are:'
  WRITE(*,'(3F8.2)') (W(i), i=1,N)

END PROGRAM dipolar_coupling
