!
! Procedures in these programs:
!
!  SUBROUTINE                 READ_XYZ               (INFILE,NBATOMS,COMMENT,ATNAMES,COORDS)
!  SUBROUTINE                 WRITE_XYZ              (OUTFILE,NBATOMS,COMMENT,ATNAMES,COORDS)
!  SUBROUTINE                 READ_FILE_NDIM_ARRAY   (INFILE,N,ARR)
!  SUBROUTINE                 WRITE_FILE_NDIM_ARRAY  (OUTFILE,ARR)
!  SUBROUTINE                 READ_FILE_INTO_STRINGS (INFILE,VEC)
!  SUBROUTINE                 WRITE_FILE_STRINGS_VEC (OUTFILE,VEC)
!  SUBROUTINE                 SBR_PATSUBST_INPLACE   (INSTRING,TO_MATCH,REPLACE)
!  SUBROUTINE                 SBR_PATSUBST           (INSTRING,TO_MATCH,REPLACE,OUTSTRING)
!  CHARACTER(LEN=80) FUNCTION FCT_PATSUBST           (INSTRING,TO_MATCH,REPLACE)
!  INTERFACE                  PRINTMAT
!  INTERFACE                  PRINTVEC
!  INTERFACE                  FCT_RMSD
!  INTERFACE                  SBR_RMSD
!  INTERFACE                  FCT_COM
!  INTERFACE                  SBR_COM
!
PROGRAM CALLER
  USE MOD_READWRITE
  USE MOD_PROCEDURES_SBR
  USE MOD_PROCEDURES_FCT
  USE MOD_INTERFACES
  implicit none

  CHARACTER(LEN=*),PARAMETER   :: INFILE='6pu0_MD.frame.1.xyz'
  INTEGER                      :: NBATOMS
  CHARACTER(LEN=80)            :: COMMENT
  CHARACTER(LEN=4),ALLOCATABLE :: ATNAMES(:)
  REAL,            ALLOCATABLE :: COORDS(:,:)

  REAL,ALLOCATABLE :: COORDS_BOHR(:,:)
  CHARACTER(LEN=80) :: U = 'first_frame.dat'
  REAL :: RMSD,COM(3)

  CALL READ_XYZ(INFILE,NBATOMS,COMMENT,ATNAMES,COORDS)

  ALLOCATE(COORDS_BOHR(NBATOMS,3))
  COORDS_BOHR = COORDS/0.529
  CALL WRITE_XYZ('coords_bohr.xyz',NBATOMS,'frame 1 converted to Bohr',ATNAMES,COORDS_BOHR)

  !CALL PRINTVEC(ATNAMES)

  ! Comparing the SBR and FCT methods of calc. a RMSD of 3D data
  CALL SBR_RMSD(COORDS_BOHR,COORDS,RMSD)
  WRITE(*,*) RMSD, FCT_RMSD(COORDS_BOHR,COORDS)

  ! Comparing the SBR and FCT methods of calc. a RMSD of 1D data
  CALL SBR_RMSD(COORDS_BOHR(:,1),COORDS(:,1),RMSD)
  WRITE(*,*) RMSD, FCT_RMSD(COORDS_BOHR(:,1),COORDS(:,1))

  ! Comparing the SBR and FCT methods of calc. a COM of 3D data
  CALL SBR_COM(COORDS_BOHR,COM)
  WRITE(*,*) COM, FCT_COM(COORDS_BOHR)

  ! Comparing the SBR and FCT methods of calc. a COM of 1D data
  CALL SBR_COM(COORDS_BOHR(:,1),COM(1))
  WRITE(*,*) COM(1), FCT_COM(COORDS_BOHR(:,1))


  CALL WRITE_FILE_NDIM_ARRAY('outfile.dat',COORDS)
  DEALLOCATE(COORDS_BOHR)

  ! Two types of substitutions: 
  CALL PATSUBST_INPLACE(U,'frame','rrrr') ; WRITE(*,*) U ! One in-place but can only act on variable strings ;
  WRITE(*,*) PATSUBST('test.dat','st','rrrr')            ! One as a function that can take non-variable input.


END PROGRAM CALLER
