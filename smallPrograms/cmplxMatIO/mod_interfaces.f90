MODULE mod_interfaces
  USE mod_io
  USE mod_procedures
  implicit none

!
! ----------
!
  INTERFACE readMatFromFile
    MODULE PROCEDURE readRMatrixFromFile,readCMatrixFromFile
  END INTERFACE readMatFromFile

  INTERFACE writeMatToFile
    MODULE PROCEDURE writeCMatToFile,writeRMatToFile
  END INTERFACE writeMatToFile

  INTERFACE printMat ! printMat(A,message) can print COMPLEX (printCMat, COMPLEX :: A) or REAL (printRMat, REAL :: A) matrices
    MODULE PROCEDURE printCMat,printRMat
  END INTERFACE printMat

  INTERFACE fillRandomMat ! printMat(A,message) can print COMPLEX (printCMat, COMPLEX :: A) or REAL (printRMat, REAL :: A) matrices
    MODULE PROCEDURE fillRandomCMat,fillRandomRMat
  END INTERFACE fillRandomMat
!
! ----------
!
END MODULE mod_interfaces
