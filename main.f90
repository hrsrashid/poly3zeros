program main
  use poly3zerosModule
  implicit none  

  complex, dimension(3) :: zeros

  zeros = poly3zeros(cmplx(1), cmplx(-6), cmplx(11), cmplx(-6))
  
  print *, zeros

end program main


!USE ZPOCC_INT
!USE WRCRN_INT
!
!IMPLICIT   NONE
!!                                 Declare variables
!INTEGER    NDEG
!PARAMETER  (NDEG=3)
!!
!COMPLEX    COEFF(NDEG+1), ZERO(NDEG)
!!                                 Set values of COEFF
!!                                 COEFF = ( 10.0 +  0.0i )
!!                                         ( -8.0 + 12.0i )
!!                                         ( -3.0 -  6.0i )
!!                                         (  1.0 +  0.0i )
!!
!DATA COEFF/(10.0,0.0), (-8.0,12.0), (-3.0,-6.0), (1.0,0.0)/
!!
!CALL ZPOCC (COEFF, ZERO)
!!
!CALL WRCRN ('The zeros found are', ZERO, 1, NDEG, 1)
!!
!END
