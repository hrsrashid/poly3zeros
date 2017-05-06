program main
  use poly3zerosModule
  implicit none  

  complex, dimension(3) :: zeros, zeros2

  zeros = poly3zeros(cmplx(1), cmplx(-6), cmplx(11), cmplx(-6))
  ! zeros2 = ZPOCC(cmplx(1), cmplx(-6), cmplx(11), cmplx(-6))
  
  print *, zeros

end program main
