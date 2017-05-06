module poly3zerosModule
  implicit none
  public :: poly3zeros, zroot3, eq
  real :: PI = 3.14159265359
  real :: EPS = 1e-3
  real :: DELTA = 1e-5

contains
  function poly3zeros(d, a1, b1, c1)
    implicit none

    complex, dimension(3) :: poly3zeros, As, Bs
    complex :: a, b, c, a1, b1, c1, d, p, q, qu, z, alpha
    real :: AA, BB
    integer :: i

    a = a1 / d
    b = b1 / d
    c = c1 / d

    p = -a*a/3 + b
    q = 2*((a/3)**3) - a*b/3 + c

    qu = (p/3)**3 + (q/2)**2

    As = zroot3(-q/2 + sqrt(qu))
    Bs = zroot3(-q/2 - sqrt(qu))

    do i = 1, 3, 1
      AA = real(As(i))
      BB = real(Bs(i))
      z = AA*BB + p/3

      if (real(z) < DELTA .AND. aimag(z) < DELTA) then
        exit
      end if
    end do

    if (real(qu) < 0 .and. aimag(qu) < DELTA) then
      alpha = acos(-q/(2 * sqrt(-(p/3)**3))) / 3
      poly3zeros(1) =  2 * sqrt(-p/3) * cos(alpha) - a/3
      poly3zeros(2) = -2 * sqrt(-p/3) * cos(alpha + PI/3) - a/3
      poly3zeros(3) = -2 * sqrt(-p/3) * cos(alpha - PI/3) - a/3
    else
      poly3zeros(1) = AA + BB - a/3
      poly3zeros(2) = cmplx(-(AA + BB)/2,  (AA - BB)/2 * sqrt(3.)) - a/3
      poly3zeros(3) = cmplx(-(AA + BB)/2, -(AA - BB)/2 * sqrt(3.)) - a/3
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