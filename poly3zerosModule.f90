module poly3zerosModule
  implicit none
  public :: poly3zeros, zroot3, eq
  real :: PI = 3.14159265359
  real :: EPS = 1e-3

contains
  function poly3zeros(a, b, c, d)
    implicit none

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

  function zroot3(x)
    complex, dimension(3) :: zroot3
    complex :: x
    real :: radius, theta

    if (x == cmplx(0)) then
      zroot3(1) = cmplx(0)
      zroot3(2) = cmplx(0)
      zroot3(3) = cmplx(0)
      return
    end if

    if (real(x) == 0) then
      if (aimag(x) > 0) then
        theta = PI/2
      else
        theta = 3./2 * PI
      end if
    else if (aimag(x) == 0) then
      if (real(x) > 0) then
        theta = 0
      else
        theta = PI
      end if
    else
      if (real(x) > 0) then
        theta = atan(aimag(x) / real(x))
      else
        theta = PI + atan(aimag(x) / real(x))
      end if
    end if

    radius = abs(x)**(1./3)
    theta = theta / 3

    zroot3(1) = radius * cmplx(cos(theta),             sin(theta))
    zroot3(2) = radius * cmplx(cos(theta + 2./3 * PI), sin(theta + 2./3 * PI))
    zroot3(3) = radius * cmplx(cos(theta + 4./3 * PI), sin(theta + 4./3 * PI))
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
        if (used(j) == 0 .AND. abs(a(i) - b(j)) < EPS) then
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