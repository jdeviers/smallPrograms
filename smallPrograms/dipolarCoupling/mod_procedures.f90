MODULE mod_procedures
  use mod_precision
  implicit none

  contains

! ----------

  SUBROUTINE create_id_mat(n,ID)
    use mod_precision
    implicit none
!
! ID(n,n) = identity matrix.
! n = extent of the square identity matrix
!
    INTEGER(i4)             :: n,i
    INTEGER(i4),ALLOCATABLE :: ID(:,:)

    ALLOCATE(ID(n,n))
    ID=0_i4
    DO i= 1,n
      ID(i,i)=1_i4
    END DO

  END SUBROUTINE create_id_mat

! ----------

  SUBROUTINE col_row_dot_prod(A,B,C)
    use mod_precision
    implicit none

    REAL(dp),ALLOCATABLE,INTENT(in) :: A(:),B(:)
    REAL(dp),ALLOCATABLE            :: C(:,:)
    INTEGER(i4)                     :: ncol,nrow,i,j

    ncol=UBOUND(A,dim=1,kind=i4)
    nrow=UBOUND(B,dim=1,kind=i4)
    ALLOCATE(C(ncol,nrow))
    DO i=1,ncol
      DO j=1,nrow
        C(i,j) = A(i) * B(j)
      END DO
    END DO

  END SUBROUTINE col_row_dot_prod

! ----------

  SUBROUTINE point_dipole_dipole_coupling(r_ab,A)
    use mod_precision
    implicit none

    REAL(dp),INTENT(in)     :: r_ab(3)
    REAL(dp)                :: A(3,3)
    REAL(dp)                :: d,dr3
    REAL(dp),ALLOCATABLE    :: e(:),Atmp(:,:)
    INTEGER(i4),ALLOCATABLE :: IDmat(:,:)

    dr3 = ( ((-4.*pi*1.E-7)/(4.*pi*1.E-30)) * ((g_e*mu_b)**2._dp) ) / ( 1.E+6 * h)
! Last term converts from SI to MHz*(Ang^3)
    d   = dr3/(NORM2(r_ab))**3.
    WRITE(*,*) "d = ",d
    ALLOCATE(e(3))
    e(:) = r_ab(:)/NORM2(r_ab)
    CALL col_row_dot_prod(e,e,Atmp)
    CALL create_id_mat(3_i4,IDmat)
    A    = d * (3.*Atmp - IDmat)

    DEALLOCATE(IDmat,e)
  END SUBROUTINE point_dipole_dipole_coupling

! ----------

END MODULE mod_procedures
