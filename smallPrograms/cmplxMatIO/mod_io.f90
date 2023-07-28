MODULE mod_io
  implicit none

  contains
!
! ----------
!
  SUBROUTINE checkFileIO(io,infile)

    INTEGER,         INTENT(IN)             :: io
    CHARACTER(LEN=*),INTENT(INOUT),OPTIONAL :: infile

    IF (io.GT.0) THEN
      ERROR STOP "*** ERROR OPENING FILE ***"
    ELSE IF (io.LT.0) THEN
      ERROR STOP "*** FILE WAS EMPTY ***"
    ELSE
      IF (PRESENT(infile)) THEN 
        WRITE(*,'(A,A)') infile,' opened succesfully.'
      ELSE
        WRITE(*,'(A)')      'File opened succesfully.'
      END IF    
    END IF

  END SUBROUTINE checkFileIO
!
! ----------
!
  SUBROUTINE readRMatrixFromFile(infile,A)

    ! .. Parameters ..
    CHARACTER(LEN=*),INTENT(INOUT) :: infile
    REAL,ALLOCATABLE,INTENT(OUT)   :: A(:,:)
    ! .. Other vars ..
    INTEGER :: i,io
    INTEGER :: nbLines
    
    OPEN(10,file=infile,status='OLD',action='READ',iostat=io)
    CALL checkFileIO(io,infile)

    nbLines = 0
    DO WHILE (io.EQ.0)
      READ(10,*,iostat=io)
      nbLines = nbLines + 1
    END DO
    nbLines = nbLines - 1
    WRITE(*,*) 'nbLines = ',nbLines

    ALLOCATE(A(nbLines,nbLines))
    REWIND(10)
    DO i=1,nbLines
      READ(10,*) A(i,:)
    END DO
    CLOSE(10)

  END SUBROUTINE readRMatrixFromFile
!
! ----------
!
  SUBROUTINE readCMatrixFromFile(infile,A)

    ! .. Parameters ..
    CHARACTER(LEN=*),   INTENT(INOUT) :: infile
    COMPLEX,ALLOCATABLE,INTENT(OUT)   :: A(:,:)
    ! .. Other vars ..
    CHARACTER(LEN=*),PARAMETER :: fmt = '(*(F8.2,SP,F8.2))'
    INTEGER :: i,io
    INTEGER :: nbLines
    
    OPEN(10,file=infile,status='OLD',action='READ',iostat=io)
    CALL checkFileIO(io,infile)

    nbLines = 0
    DO WHILE (io.EQ.0)
      READ(10,*,iostat=io)
      nbLines = nbLines + 1
    END DO
    nbLines = nbLines - 1
    WRITE(*,*) 'nbLines = ',nbLines

    ALLOCATE(A(nbLines,nbLines))
    REWIND(10)

    DO i=1,nbLines
      READ(10,fmt) A(i,:)
    END DO
    CLOSE(10)

  END SUBROUTINE readCMatrixFromFile
!
! ----------
!
  SUBROUTINE readIMatrixFromFile(infile,A)

    ! .. Parameters ..
    CHARACTER(LEN=*),   INTENT(INOUT)  :: infile
    INTEGER,ALLOCATABLE,INTENT(OUT)    :: A(:,:)
    ! .. Other vars ..
    INTEGER :: i,io
    INTEGER :: nbLines
    
    OPEN(10,file=infile,status='OLD',action='READ',iostat=io)
    CALL checkFileIO(io,infile)

    nbLines = 0
    DO WHILE (io.EQ.0)
      READ(10,*,iostat=io)
      nbLines = nbLines + 1
    END DO
    nbLines = nbLines - 1
    WRITE(*,*) 'nbLines = ',nbLines

    ALLOCATE(A(nbLines,nbLines))
    REWIND(10)
    DO i=1,nbLines
      READ(10,*) A(i,:)
    END DO
    CLOSE(10)

  END SUBROUTINE readIMatrixFromFile
!
! ----------
!
  SUBROUTINE writeRMatToFile(outfile,A,message)

    CHARACTER(LEN=*),INTENT(INOUT)       :: outfile
    REAL,            INTENT(IN)          :: A(:,:)
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: message
    INTEGER :: i,j 
    CHARACTER(LEN=*),PARAMETER :: fmt = '(*(F10.4))'

    OPEN(11,file=outfile,status='unknown',action='write')

    IF (PRESENT(message)) WRITE(11,'(A)') message
    DO i = 1,UBOUND(A,1)
      WRITE(11,fmt) (A(i,j), j=1,UBOUND(A,2))
    END DO

    CLOSE(11)

  END SUBROUTINE writeRMatToFile
!
! ----------
!
  SUBROUTINE writeCMatToFile(outfile,A,message)

    CHARACTER(LEN=*),INTENT(INOUT)       :: outfile
    COMPLEX,         INTENT(IN)          :: A(:,:)
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: message
    INTEGER :: i,j 
    CHARACTER(LEN=*),PARAMETER :: fmt = '(*(F8.2,SP,F8.2))'

    OPEN(11,file=outfile,status='unknown',action='write')

    IF (PRESENT(message)) WRITE(11,'(A)') message
    DO i = 1,UBOUND(A,1)
      WRITE(11,fmt) (A(i,j), j=1,UBOUND(A,2))
    END DO

    CLOSE(11)

  END SUBROUTINE writeCMatToFile

END MODULE mod_io
