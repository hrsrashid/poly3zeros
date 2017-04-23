test_suite poly3zerosModule

test comparator
  assert_true(eq((/cmplx(1), cmplx(3), cmplx(2)/), (/cmplx(3), cmplx(2), cmplx(1)/)))
  assert_true(eq((/cmplx(1), cmplx(1), cmplx(1)/), (/cmplx(1), cmplx(1), cmplx(1)/)))
  assert_false(eq((/cmplx(0, 1), cmplx(3), cmplx(2)/), (/cmplx(3), cmplx(2), cmplx(1)/)))
end test

test cmplxRoots
  complex, dimension(3) :: roots, correct = (/cmplx(0, 1), cmplx(0, 2), cmplx(0, 3)/)

  roots = poly3zeros(cmplx(1), cmplx(0, -6), cmplx(-11), cmplx(0, 6))

  assert_true(eq(roots,correct))
end test

test realRoots
  complex, dimension(3) :: roots, correct = (/cmplx(1), cmplx(2), cmplx(3)/)

  roots = poly3zeros(cmplx(1), cmplx(-6), cmplx(11), cmplx(-6))

  assert_true(eq(roots,correct))
end test

test cmplxFractionalRoots
  complex, dimension(3) :: roots, correct = (/cmplx(0, 1), cmplx(0, 1.001), cmplx(0, 1.0001)/)

  roots = poly3zeros(cmplx(1), cmplx(0, -3.0011), cmplx(-3.0022), cmplx(0, 1.0011))

  assert_true(eq(roots,correct))
end test

test rang2root
  complex, dimension(3) :: roots, correct = (/cmplx(1), cmplx(1), cmplx(2)/)

  roots = poly3zeros(cmplx(1), cmplx(-4), cmplx(5), cmplx(-2))

  assert_true(eq(roots,correct))
end test

test rang3root
  complex, dimension(3) :: roots, correct = (/cmplx(1), cmplx(1), cmplx(1)/)

  roots = poly3zeros(cmplx(1), cmplx(-3), cmplx(3), cmplx(-1))

  assert_true(eq(roots,correct))
end test

end test_suite