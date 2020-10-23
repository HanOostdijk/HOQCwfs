test_that("WFS_tags checks", {
  a1 = bg('a12','b34')
  b1 = '<a12>b34</a12>'
  expect_identical(a1,b1)
  a2 = bg('a12')
  b2 = '<a12></a12>'
  expect_identical(a2,b2)
  a3 = fg('a12','b34','c56',sep='x')
  b3 = '<a12>xb34xc56x</a12>'
  expect_identical(a3,b3)
  a3 = fg('a12',sep='x','b34','c56')
  b3 = '<a12>xb34xc56x</a12>'
  expect_identical(a3,b3)
  a4 = fg('a12',sep='x','b34','c56',ta='(x=y)')
  b4 = '<a12 (x=y)>xb34xc56x</a12>'
  expect_identical(a4,b4)
  a4 = fg('a12',ta='(x=y)',sep='x','b34','c56')
  b4 = '<a12 (x=y)>xb34xc56x</a12>'
  expect_identical(a4,b4)
})
