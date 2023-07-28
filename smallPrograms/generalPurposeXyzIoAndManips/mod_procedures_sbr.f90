MODULE MOD_PROCEDURES_SBR
  implicit none

  contains

  SUBROUTINE SBR_RMSD_VEC(COORDS_REF,COORDS,RMSD)
    implicit none

    ! .. Parameters ..
    REAL,INTENT(IN)  :: COORDS_REF(:),COORDS(:)
    REAL,INTENT(OUT) :: RMSD

    ! .. Internal vars ..
    INTEGER :: NBCOORDS,i
    REAL    :: ACCDIFF

    IF (UBOUND(COORDS_REF,1).NE.UBOUND(COORDS,1)) ERROR STOP "RMSD FAILED: MISMATCHED COORDS"
    NBCOORDS = UBOUND(COORDS_REF,1)

    ACCDIFF=0.
    DO i=1,NBCOORDS
      ACCDIFF=ACCDIFF+(COORDS(i)-COORDS_REF(i))**2.
    END DO
    RMSD = SQRT(ACCDIFF/NBCOORDS)

  END SUBROUTINE SBR_RMSD_VEC

  SUBROUTINE SBR_RMSD_MAT(COORDS_REF,COORDS,RMSD)
    implicit none

    ! .. Parameters ..
    REAL,INTENT(IN)  :: COORDS_REF(:,:),COORDS(:,:)
    REAL,INTENT(OUT) :: RMSD

    ! .. Internal vars ..
    INTEGER :: NBCOORDS,NBDIMS,i,j
    REAL    :: LINEDIST,ACCDIFF

    IF (UBOUND(COORDS_REF,1).NE.UBOUND(COORDS,1)) ERROR STOP "RMSD FAILED: MISMATCHED COORDS"
    IF (UBOUND(COORDS_REF,2).NE.UBOUND(COORDS,2)) ERROR STOP "RMSD FAILED: MISMATCHED COORDS"
    NBCOORDS = UBOUND(COORDS_REF,1)
    NBDIMS =   UBOUND(COORDS_REF,2)

    ACCDIFF=0.
    DO i=1,NBCOORDS
      LINEDIST=0.
      DO j=1,NBDIMS
        LINEDIST=LINEDIST + (COORDS(i,j)-COORDS_REF(i,j))**2.
      END DO
      ACCDIFF=ACCDIFF+LINEDIST
    END DO
    RMSD = SQRT(ACCDIFF/NBCOORDS)

  END SUBROUTINE SBR_RMSD_MAT

  SUBROUTINE SBR_COM_VEC(COORDS,COM)
    implicit none

    ! .. Parameters ..
    REAL,INTENT(IN)  :: COORDS(:)
    REAL,INTENT(OUT) :: COM

    ! .. Internal vars ..
    INTEGER :: NBCOORDS,i
    REAL    :: ACCSUM
    
    NBCOORDS=UBOUND(COORDS,1)
    ACCSUM = 0.

    DO i=1,NBCOORDS
      ACCSUM = ACCSUM+COORDS(i)
    END DO
    COM = ACCSUM/NBCOORDS

  END SUBROUTINE SBR_COM_VEC

  SUBROUTINE SBR_COM_MAT(COORDS,COM)
    implicit none

    ! .. Parameters ..
    REAL,             INTENT(IN)  :: COORDS(:,:)
    REAL,DIMENSION(3),INTENT(OUT) :: COM

    ! .. Internal vars ..
    INTEGER :: NBCOORDS,i
    REAL    :: ACCSUM(3)
    
    NBCOORDS=UBOUND(COORDS,1)
    ACCSUM = 0.

    DO i=1,NBCOORDS
      ACCSUM(:) = ACCSUM(:)+COORDS(i,:)
    END DO
    COM = ACCSUM/NBCOORDS

  END SUBROUTINE SBR_COM_MAT

END MODULE MOD_PROCEDURES_SBR