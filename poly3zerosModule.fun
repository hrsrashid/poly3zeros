test_suite poly3zerosModule

test comparator
  assert_true(eq((/cmplx(1), cmplx(3), cmplx(2)/), (/cmplx(3), cmplx(2), cmplx(1)/)))
  assert_true(eq((/cmplx(1), cmplx(1), cmplx(1)/), (/cmplx(1), cmplx(1), cmplx(1)/)))
  assert_false(eq((/cmplx(0, 1), cmplx(3), cmplx(2)/), (/cmplx(3), cmplx(2), cmplx(1)/)))
end test

test zroot3_zero
  assert_true(eq(zroot3(cmplx(0)),(/cmplx(0), cmplx(0), cmplx(0)/)))
end test

test zroot3_right
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(1))
  correct = (/cmplx(1), cmplx(-0.5, 0.86603), cmplx(-0.5, -0.86603)/)
  assert_true(eq(roots,correct))
end test

test zroot3_left
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(-1))
  correct = (/cmplx(-1), cmplx(0.5, 0.86603), cmplx(0.5, -0.86603)/)
  assert_true(eq(roots,correct))
end test

test zroot3_top
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(0, 1))
  correct = (/cmplx(0, -1), cmplx(0.86603, 0.5), cmplx(-0.86603, 0.5)/)
  assert_true(eq(roots,correct))
end test

test zroot3_bottom
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(0, -1))
  correct = (/cmplx(0, 1), cmplx(0.86603, -0.5), cmplx(-0.86603, -0.5)/)
  assert_true(eq(roots,correct))
end test

test zroot3_q1
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(2, 3))
  correct = (/cmplx(1.4519, 0.4934), cmplx(-1.1532, 1.0106), cmplx(-0.2986, -1.5040)/)
  assert_true(eq(roots,correct))
end test

test zroot3_q2
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(-46, 9))
  correct = (/cmplx(2, 3), cmplx(-3.5981, 0.2321), cmplx(1.5981, -3.2321)/)
  assert_true(eq(roots,correct))
end test

test zroot3_q3
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(-1, -1))
  correct = (/cmplx(0.79370, -0.79370), cmplx(0.29051, 1.08422), cmplx(-1.08422, -0.29051)/)
  assert_true(eq(roots,correct))
end test

test zroot3_q4
  complex, dimension(3) :: roots, correct
  roots = zroot3(cmplx(1, -1))
  correct = (/cmplx(1.08422, -0.29051), cmplx(-0.29051, 1.08422), cmplx(-0.79370, -0.79370)/)
  assert_true(eq(roots,correct))
end test

test cmplxRoots
  complex, dimension(3) :: roots, correct
  correct = (/cmplx(0, 1), cmplx(0, 2), cmplx(0, 3)/)
  roots = poly3zeros(cmplx(1), cmplx(0, -6), cmplx(-11), cmplx(0, 6))
  assert_true(eq(roots,correct))
end test

test realRoots
  complex, dimension(3) :: roots, correct
  correct = (/cmplx(1), cmplx(2), cmplx(3)/)
  roots = poly3zeros(cmplx(1), cmplx(-6), cmplx(11), cmplx(-6))
  assert_true(eq(roots,correct))
end test

test cmplxFractionalRoots
  complex, dimension(3) :: roots, correct
  correct = (/cmplx(0, 1), cmplx(0, 1.001), cmplx(0, 1.0001)/)
  roots = poly3zeros(cmplx(1), cmplx(0, -3.0011), cmplx(-3.0022), cmplx(0, 1.0011))
  assert_true(eq(roots,correct))
end test

test rang2root
  complex, dimension(3) :: roots, correct
  correct = (/cmplx(1), cmplx(1), cmplx(2)/)
  roots = poly3zeros(cmplx(1), cmplx(-4), cmplx(5), cmplx(-2))
  assert_true(eq(roots,correct))
end test

test rang3root
  complex, dimension(3) :: roots, correct
  correct = (/cmplx(1), cmplx(1), cmplx(1)/)
  roots = poly3zeros(cmplx(1), cmplx(-3), cmplx(3), cmplx(-1))
  assert_true(eq(roots,correct))
end test

test realAndCmplxRoots
  complex, dimension(3) :: roots, correct
  correct = (/cmplx(1), cmplx(0, 1), cmplx(0, -1)/)
  roots = poly3zeros(cmplx(1), cmplx(-1), cmplx(1), cmplx(-1))
  assert_true(eq(roots,correct))
end test

end test_suite