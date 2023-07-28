MODULE MOD_READWRITE
  implicit none

  contains


  ! -- READS A .XYZ FILE --
  SUBROUTINE READ_XYZ(INFILE,NBATOMS,COMMENT,ATNAMES,COORDS)
    implicit none

    ! .. Params ..
    CHARACTER(LEN=*),            INTENT(IN)  :: INFILE
    INTEGER,                     INTENT(OUT) :: NBATOMS
    CHARACTER(LEN=80),           INTENT(OUT) :: COMMENT
    CHARACTER(LEN=4),ALLOCATABLE,INTENT(OUT) :: ATNAMES(:)
    REAL,            ALLOCATABLE,INTENT(OUT) :: COORDS(:,:)

    ! .. Internal vars ..
    INTEGER :: i,io

    OPEN(10,FILE=INFILE,STATUS='OLD',ACTION='READ',IOSTAT=IO)
    IF (io.NE.0) ERROR STOP "FILE NONEXISTENT OR EMPTY."

    READ(10,*,IOSTAT=io) NBATOMS
    READ(10,*,IOSTAT=io) COMMENT

    ALLOCATE(ATNAMES(NBATOMS))
    ALLOCATE(COORDS(NBATOMS,3))

    DO i=1,NBATOMS
      READ(10,*,IOSTAT=io) ATNAMES(i),COORDS(i,:)
      IF (io.NE.0) ERROR STOP "ERROR READING FILE: WRONG NBATOMS OR CORRUPTED."
    END DO

    CLOSE(10)

  END SUBROUTINE READ_XYZ
  ! ---------------------

  ! -- WRITES AN .XYZ FILE --
  SUBROUTINE WRITE_XYZ(OUTFILE,NBATOMS,COMMENT,ATNAMES,COORDS)
    implicit none

    ! .. Params ..
    CHARACTER(LEN=*),INTENT(IN) :: OUTFILE
    INTEGER,         INTENT(IN) :: NBATOMS
    CHARACTER(LEN=*),INTENT(IN) :: COMMENT
    CHARACTER(LEN=4),INTENT(IN) :: ATNAMES(:)
    REAL,            INTENT(IN) :: COORDS(:,:)

    ! .. Internal vars ..
    INTEGER :: i,io

    OPEN(unit=10,file=OUTFILE,status='UNKNOWN',action='WRITE',iostat=io)
    IF (io.NE.0) ERROR STOP "ERROR OPENING FILE FOR WRITING."

    WRITE(10,'(I8)') NBATOMS
    WRITE(10,'(A)')  COMMENT

    DO i=1,NBATOMS
      WRITE(10,'(A4,*(F8.3))') ATNAMES(i),COORDS(i,:)
    END DO
    CLOSE(10)

  END SUBROUTINE WRITE_XYZ
  ! ----------------------------------


  ! -- READS A N-DIMENSIONAL REAL ARRAY FROM FILE --
  SUBROUTINE READ_FILE_NDIM_ARRAY(INFILE,N,ARR)
    implicit none

    ! .. Parameters ..
    CHARACTER(LEN=*),INTENT(IN)  :: INFILE
    INTEGER,         INTENT(IN)  :: N
    REAL,ALLOCATABLE,INTENT(OUT) :: ARR(:,:)

    ! .. Internal vars ..
    INTEGER                      :: io,i
    INTEGER                      :: NBROWS

    OPEN(unit=10,file=INFILE,status='OLD',action='READ',iostat=io)
    IF (io.NE.0) ERROR STOP 'FILE NONEXISTENT OR EMPTY.'

    NBROWS = 0
    DO WHILE (io.EQ.0)
      READ(10,*,iostat=io)
      NBROWS = NBROWS + 1
    END DO
    NBROWS = NBROWS - 1 ! Always needed, not sure why
    WRITE(*,'(A,I0)') 'NBROWS = ',NBROWS

    ALLOCATE(ARR(NBROWS,N))
    REWIND(10)

    DO i = 1,NBROWS
      READ(10,*,IOSTAT=io) ARR(i,:)
    END DO

    CLOSE(10)

  END SUBROUTINE READ_FILE_NDIM_ARRAY
  ! ---------------------------------


  ! -- WRITES A N-DIMENSIONAL REAL ARRAY TO FILE --
  SUBROUTINE WRITE_FILE_NDIM_ARRAY(OUTFILE,ARR)
    implicit none

    ! .. Parameters ..
    CHARACTER(LEN=*),INTENT(IN) :: OUTFILE
    REAL,            INTENT(IN) :: ARR(:,:)

    ! .. Internal vars ..
    INTEGER                     :: i,io

    OPEN(unit=10,file=OUTFILE,status='UNKNOWN',action='WRITE',iostat=io)
    IF (io.NE.0) ERROR STOP "ERROR OPENING FILE FOR WRITING."
    DO i=1,UBOUND(ARR,1)
      WRITE(10,'(*(F8.3))') ARR(i,:)
    END DO
    CLOSE(10)

  END SUBROUTINE WRITE_FILE_NDIM_ARRAY
  ! ----------------------------------


  ! -- READS A FILE INTO A VECTOR OF 80-LONG STRINGS --
  SUBROUTINE READ_FILE_INTO_STRINGS(INFILE,VEC)
    implicit none

    ! .. Parameters ..
    CHARACTER(LEN=*),             INTENT(IN)  :: INFILE
    CHARACTER(LEN=80),ALLOCATABLE,INTENT(OUT) :: VEC(:)

    ! .. Internal vars ..
    INTEGER                      :: io,i
    INTEGER                      :: NBROWS

    OPEN(unit=10,file=INFILE,status='OLD',action='READ',iostat=io)
    IF (io.NE.0) ERROR STOP 'FILE NONEXISTENT OR EMPTY.'

    NBROWS = 0
    DO WHILE (io.EQ.0)
      READ(10,*,iostat=io)
      NBROWS = NBROWS + 1
    END DO
    NBROWS = NBROWS - 1 ! Always needed, not sure why
    WRITE(*,'(A,I0)') 'NBROWS = ',NBROWS

    ALLOCATE(VEC(NBROWS))
    REWIND(10)

    DO i = 1,NBROWS
      READ(10,*,IOSTAT=io) VEC(i)
    END DO

    CLOSE(10)

  END SUBROUTINE READ_FILE_INTO_STRINGS
  ! ---------------------------------

  ! -- WRITES A VECTOR OF STRINGS TO FILE --
  SUBROUTINE WRITE_FILE_STRINGS_VEC(OUTFILE,VEC)
    implicit none

    ! .. Parameters ..
    CHARACTER(LEN=*), INTENT(IN) :: OUTFILE
    CHARACTER(LEN=80),INTENT(IN) :: VEC(:)

    ! .. Internal vars ..
    INTEGER :: i,io

    OPEN(unit=10,file=OUTFILE,status='UNKNOWN',action='WRITE',iostat=io)
    IF (io.NE.0) ERROR STOP "ERROR OPENING FILE FOR WRITING."
    DO i=1,UBOUND(VEC,1)
      WRITE(10,'(A)') VEC(i)
    END DO
    CLOSE(10)

  END SUBROUTINE WRITE_FILE_STRINGS_VEC
  ! ----------------------------------


  ! -- PRINTS A REAL ARRAY --
  SUBROUTINE PRINTMAT_R(A,message)
    implicit none

    ! .. Arguments ..
    REAL,                     INTENT(IN) :: A(:,:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: message
    ! .. Control var ..
    INTEGER :: i

    WRITE(*,*)
    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i=1,UBOUND(A,1)
      WRITE(*,'(*(F8.3))') (A(i,:))
    END DO

  END SUBROUTINE PRINTMAT_R
  ! -----------------------

  ! -- PRINTS AN INTEGER ARRAY --
  SUBROUTINE PRINTMAT_I(A,message)
    implicit none

    ! .. Arguments ..
    INTEGER,                  INTENT(IN) :: A(:,:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: message
    ! .. Control var ..
    INTEGER :: i

    WRITE(*,*)
    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i=1,UBOUND(A,1)
      WRITE(*,'(*(I8))') (A(i,:))
    END DO

  END SUBROUTINE PRINTMAT_I
  ! -----------------------

  ! -- PRINTS A CHARACTER ARRAY --
  SUBROUTINE PRINTMAT_C(A,message)
    implicit none

    ! .. Arguments ..
    CHARACTER(LEN=*),         INTENT(IN) :: A(:,:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: message
    ! .. Control var ..
    INTEGER :: i

    WRITE(*,*)
    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i=1,UBOUND(A,1)
      WRITE(*,'(*(A))') (A(i,:))
    END DO

  END SUBROUTINE PRINTMAT_C
  ! -----------------------

  ! -- PRINTS A REAL ARRAY --
  SUBROUTINE PRINTVEC_R(A,message)
    implicit none

    ! .. Arguments ..
    REAL,                     INTENT(IN) :: A(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: message
    ! .. Control var ..
    INTEGER :: i

    WRITE(*,*)
    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i=1,UBOUND(A,1)
      WRITE(*,'(F8.3)') (A(i))
    END DO

  END SUBROUTINE PRINTVEC_R
  ! -----------------------

  ! -- PRINTS AN INTEGER ARRAY --
  SUBROUTINE PRINTVEC_I(A,message)
    implicit none

    ! .. Arguments ..
    INTEGER,                  INTENT(IN) :: A(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: message
    ! .. Control var ..
    INTEGER :: i

    WRITE(*,*)
    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i=1,UBOUND(A,1)
      WRITE(*,'(I8)') (A(i))
    END DO

  END SUBROUTINE PRINTVEC_I
  ! -----------------------

  ! -- PRINTS A CHARACTER ARRAY --
  SUBROUTINE PRINTVEC_C(A,message)
    implicit none

    ! .. Arguments ..
    CHARACTER(LEN=*),         INTENT(IN) :: A(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: message
    ! .. Control var ..
    INTEGER :: i

    WRITE(*,*)
    IF (PRESENT(message)) WRITE(*,'(A)') message
    DO i=1,UBOUND(A,1)
      WRITE(*,'(A)') TRIM(A(i))
    END DO

  END SUBROUTINE PRINTVEC_C
  ! -----------------------

  ! -- REPLACE A PATTERN IN A STRING.
  ! -- CANNOT BE A PARAMETER OR A CONSTANT EXPR THOUGH.
  SUBROUTINE SBR_PATSUBST_INPLACE(INSTRING,TO_MATCH,REPLACE)
    implicit none

    ! .. Parameters ..
    CHARACTER(LEN=*),INTENT(INOUT) :: INSTRING
    CHARACTER(LEN=*),INTENT(IN)    :: TO_MATCH,REPLACE

    ! .. Internal vars ..
    INTEGER :: start,end

    start = (INDEX(INSTRING,TO_MATCH)-1)
    end   = start+LEN(TO_MATCH)+1
    INSTRING = TRIM(INSTRING(1:start)//REPLACE//INSTRING(end:))

  END SUBROUTINE SBR_PATSUBST_INPLACE

  SUBROUTINE SBR_PATSUBST(INSTRING,TO_MATCH,REPLACE,OUTSTRING)
    implicit none

    ! .. Parameters ..
    CHARACTER(LEN=*),INTENT(IN)   :: INSTRING
    CHARACTER(LEN=*),INTENT(IN)   :: TO_MATCH,REPLACE
    CHARACTER(LEN=80),INTENT(OUT) :: OUTSTRING

    ! .. Internal vars ..
    INTEGER :: start,end

    start = (INDEX(INSTRING,TO_MATCH)-1)
    end   = start+LEN(TO_MATCH)+1
    OUTSTRING = TRIM(INSTRING(1:start)//REPLACE//INSTRING(end:))

  END SUBROUTINE SBR_PATSUBST

  CHARACTER(LEN=80) FUNCTION FCT_PATSUBST(INSTRING,TO_MATCH,REPLACE)
    implicit none
    
    ! .. Parameters ..
    CHARACTER(LEN=*),INTENT(IN) :: INSTRING,TO_MATCH,REPLACE
    
    ! .. Internal vars ..
    INTEGER :: start,end

    start = (INDEX(INSTRING,TO_MATCH)-1)
    end   = start+LEN(TO_MATCH)+1
    FCT_PATSUBST = TRIM(INSTRING(1:start)//REPLACE//INSTRING(end:))

  END FUNCTION FCT_PATSUBST

END MODULE MOD_READWRITE
