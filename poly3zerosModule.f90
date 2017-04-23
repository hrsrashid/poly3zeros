module poly3zerosModule
  implicit none
  public :: poly3zeros, eq

contains

  function poly3zeros(a, b, c, d)
    implicit none

    real :: PI = 3.14159265359
    complex, dimension(3) :: poly3zeros
    complex :: a, b, c, d, p, q, qu, alpha, beta, phi, psi

    p = (3*a*c - b**2) / (3*(a**2))
    q = (2*(b**3) - 9*a*b*c + 27*(a**2)*d) / (27*(a**3))

    qu = (p/3)**3 + (q/2)**2

    if ( real(qu) < 0 ) then
      psi = sqrt(-qu) / (-q/2)

      if ( real(q) < 0 ) then
        phi = atan(psi)
      else if ( real(q)  > 0 ) then
        phi = atan(psi) + PI
      else 
        phi = PI/2
      end if

      poly3zeros(1) = 2 * sqrt(-p/3) * cos(phi/3) - b/(3*a)
      poly3zeros(2) = 2 * sqrt(-p/3) * cos(phi/3 + 2*PI/3) - b/(3*a)
      poly3zeros(3) = 2 * sqrt(-p/3) * cos(phi/3 + 4*PI/3) - b/(3*a)
    else if ( real(qu) > 0 ) then
      alpha = (-q/2 + sqrt(qu))**(1./3)
      beta = (-q/2 - sqrt(qu))**(1./3)

      poly3zeros(1) = alpha + beta - b/(3*a)
      poly3zeros(2) = cmplx(-1./2 * real(alpha + beta),  sqrt(.3)/2 * real(alpha - beta)) - b/(3*a)
      poly3zeros(3) = cmplx(-1./2 * real(alpha + beta), -sqrt(.3)/2 * real(alpha - beta)) - b/(3*a)
    else
      poly3zeros(1) = 2 * (-q/2)**(1./3) - b/(3*a)
      poly3zeros(2) =   - (-q/2)**(1./3) - b/(3*a)
      poly3zeros(3) =                    - b/(3*a)
    end if
  end function

  function eq(a, b)
    implicit none
    LOGICAL :: eq
    complex, dimension(3) :: a, b
    integer :: i, j
    integer, dimension(3) :: used
    
    used = (/0, 0, 0/)
    
    do i = 1, 3, 1
      do j = 1, 3, 1
        if (used(j) == 0 .AND. a(i) == b(j)) then
          used(j) = 1
          exit
        end if
      end do
    end do

    eq = .TRUE.

    do i = 1, 3, 1
      eq = eq .AND. used(i) == 1
    end do
  end function

end module poly3zerosModule