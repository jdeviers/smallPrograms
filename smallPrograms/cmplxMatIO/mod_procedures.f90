MODULE mod_procedures
  implicit none

  contains
!
  SUBROUTINE printCMat(A,message)

    COMPLEX,         INTENT(IN)          :: A(:,:)
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: message
    INTEGER :: i,j 
    CHARACTER(LEN=*),PARAMETER :: fmt = '(*(F0.2,SP,F0.2,"i",3X))'

    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i = 1,UBOUND(A,1)
      WRITE(*,fmt) (A(i,j), j=1,UBOUND(A,2))
    END DO

  END SUBROUTINE printCMat
!
  SUBROUTINE printRMat(A,message)

    REAL,            INTENT(IN)          :: A(:,:)
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: message
    INTEGER :: i,j 
    CHARACTER(LEN=*),PARAMETER :: fmt = '(*(F10.4))'

    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i = 1,UBOUND(A,1)
      WRITE(*,fmt) (A(i,j), j=1,UBOUND(A,2))
    END DO

  END SUBROUTINE printRMat
!
  SUBROUTINE fillRandomRMat(A,negVals,multiplier)

    REAL,     INTENT(INOUT)       :: A(:,:)
    CHARACTER,INTENT(IN),OPTIONAL :: negVals
    REAL,     INTENT(IN),OPTIONAL :: multiplier

    CALL RANDOM_NUMBER(A)
    IF (PRESENT(negVals))    A = A - 0.5
    IF (PRESENT(multiplier)) A = A * multiplier

  END SUBROUTINE fillRandomRMat
!
  SUBROUTINE fillRandomCMat(A,negVals,multiplier)

    COMPLEX,INTENT(INOUT)         :: A(:,:)
    CHARACTER,INTENT(IN),OPTIONAL :: negVals
    REAL,     INTENT(IN),OPTIONAL :: multiplier
    REAL,ALLOCATABLE              :: AR(:,:),AI(:,:)
    INTEGER                       :: a1,a2
    INTEGER                       :: i,j 
    
    a1 = UBOUND(A,1) ; a2 = UBOUND(A,2)
    ALLOCATE( AR(a1,a2), AI(a1,a2) )
    CALL RANDOM_NUMBER(AR) ; CALL RANDOM_NUMBER(AI)
    DO i=1,a1
      DO j=1,a2
        A(i,j) = CMPLX(AR(i,j),AI(i,j))
      END DO
    END DO
    DEALLOCATE(AR,AI)

    IF (PRESENT(negVals))    A = A - 0.5
    IF (PRESENT(multiplier)) A = A * multiplier

  END SUBROUTINE fillRandomCMat
!

END MODULE mod_procedures