program main
  use poly3zerosModule
  implicit none

  write(*,*)

  print *, "(z - i)(z - 2i)(z - 3i) = 0"
  call prettyPrint("correct ", (/cmplx(0, 1), cmplx(0, 2), cmplx(0, 3)/))
  call prettyPrint("result  ", poly3zeros(cmplx(1), cmplx(0, -6), cmplx(-11), cmplx(0, 6)))
  write(*,*)

  print *, "(z - 1)(z - 2)(z - 3) = 0"
  call prettyPrint("correct ", (/cmplx(1), cmplx(2), cmplx(3)/))
  call prettyPrint("result  ", poly3zeros(cmplx(1), cmplx(-6), cmplx(11), cmplx(-6)))
  write(*,*)

  print *, "(z - i)(z - 1.001i)(z - 1.0001i) = 0"
  call prettyPrint("correct ", (/cmplx(0, 1), cmplx(0, 1.001), cmplx(0, 1.0001)/))
  call prettyPrint("result  ", poly3zeros(cmplx(1), cmplx(0, -3.0011), cmplx(-3.0022), cmplx(0, 1.0011)))
  write(*,*)

  print *, "(z - 1)(z - 1)(z - 2) = 0"
  call prettyPrint("correct ", (/cmplx(1), cmplx(1), cmplx(2)/))
  call prettyPrint("result  ", poly3zeros(cmplx(1), cmplx(-4), cmplx(5), cmplx(-2)))
  write(*,*)

  print *, "(z - 1)(z - 1)(z - 1) = 0"
  call prettyPrint("correct ", (/cmplx(1), cmplx(1), cmplx(1)/))
  call prettyPrint("result  ", poly3zeros(cmplx(1), cmplx(-3), cmplx(3), cmplx(-1)))
  write(*,*)

contains

  subroutine prettyPrint(prefix, zs)
    character(len=*) :: prefix
    complex, dimension(3) :: zs
    write(*, '(A, "| ", 3(S, F7.4, SP, F7.4, "i", " | "))') prefix, zs
  end subroutine

end program main
