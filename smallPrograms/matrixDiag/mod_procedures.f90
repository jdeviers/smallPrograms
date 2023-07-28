MODULE mod_procedures
  use mod_precision
  implicit  none

  contains

    SUBROUTINE COUNT_ROWS(ARG,LEN)
      implicit none

      INTEGER            :: i,io,LEN
      INTEGER,INTENT(IN) :: ARG
      CHARACTER(LEN=50)  :: infile

      LEN=-1
      CALL GETARG(ARG,infile)
      OPEN(10,file=infile,status='old',action='read',iostat=io)
      IF (io .NE. 0) ERROR STOP 'Error opening the file'
        DO WHILE (io .EQ. 0)
          READ(10,*,iostat=io)
          LEN=LEN+1
        END DO
      CLOSE(10)
    
    END SUBROUTINE COUNT_ROWS
! ----------
    SUBROUTINE READ_NxN_MAT(ARG,LEN,MAT)
!
! ARG = argument index: read the ARG-th file given as argument to the program
!
      implicit none
    
      INTEGER              :: i
      INTEGER,INTENT(IN)   :: LEN,ARG
      REAL(dp),ALLOCATABLE :: MAT(:,:)
      CHARACTER(LEN=50)    :: infile

      CALL GETARG(ARG,infile)
      OPEN(10,file=infile,status='old',action='read')
      DO i=1,LEN
        READ(10,*) MAT(i,:)
      END DO
      CLOSE(10)

    END SUBROUTINE READ_NxN_MAT
! ----------
    LOGICAL FUNCTION CHECKSYMM(MAT,LEN)
      implicit none

      INTEGER              :: i,j
      INTEGER,INTENT(IN)   :: LEN
      REAL(dp),ALLOCATABLE :: MAT(:,:)

      DO i=1,LEN
        DO j=i+1,LEN

          IF (MAT(i,j) .NE. MAT(j,i)) THEN
            CHECKSYMM=.FALSE.
            WRITE(*,'(A)') 'A is NOT SYMMETRIC; using DGEEV.'
            RETURN
          END IF
        
        END DO
      END DO
      CHECKSYMM=.TRUE.
      WRITE(*,'(A)') 'A is SYMMETRIC; using DSYEV.'
      RETURN
    END FUNCTION CHECKSYMM
! ----------
    SUBROUTINE PRINTVEC(VEC,LEN)
      implicit none

      INTEGER              :: i
      INTEGER,INTENT(IN)   :: LEN
      REAL(dp),ALLOCATABLE :: VEC(:)

      WRITE(*,'(*(F10.3))') (VEC(i), i=1,LEN)
    END SUBROUTINE PRINTVEC
! ----------
    SUBROUTINE PRINTSQMAT(MAT,LEN)
      implicit none

      INTEGER              :: i,j
      INTEGER,INTENT(IN)   :: LEN
      REAL(dp),ALLOCATABLE :: MAT(:,:)

      DO i=1,LEN
        WRITE(*,'(*(F10.3))') (MAT(i,j), j=1,LEN)
      END DO
    END SUBROUTINE PRINTSQMAT
! ----------
    SUBROUTINE PRINT_TOFILE_VEC(VEC,LEN,OUTFILE)
      implicit none

      INTEGER              :: i
      INTEGER,INTENT(IN)   :: LEN
      CHARACTER(LEN=*)     :: OUTFILE
      REAL(dp),ALLOCATABLE :: VEC(:)

      OPEN(10,file=OUTFILE,status='unknown',action='write')
      WRITE(10,'(*(F12.6))') (VEC(i), i=1,LEN)
      CLOSE(10)
    END SUBROUTINE PRINT_TOFILE_VEC
! ----------
    SUBROUTINE PRINT_TOFILE_SQMAT(MAT,LEN,OUTFILE)
      implicit none

      INTEGER              :: i,j
      INTEGER,INTENT(IN)   :: LEN
      CHARACTER(LEN=*)     :: OUTFILE
      REAL(dp),ALLOCATABLE :: MAT(:,:)

      OPEN(10,file=OUTFILE,status='unknown',action='write')
      DO i=1,LEN
        WRITE(10,'(*(F12.6))') (MAT(i,j), j=1,LEN)
      END DO
      CLOSE(10)
    END SUBROUTINE PRINT_TOFILE_SQMAT
! ----------
    SUBROUTINE MKDIR
      implicit none

    END SUBROUTINE MKDIR

END MODULE mod_procedures
