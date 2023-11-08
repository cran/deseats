### Test 'seqCpp'

test_that("seqCpp returns a 1-column-matrix", {
  expect_equal(dim(seqCpp(from = 1, to = 10)), dim(matrix(1:10, ncol = 1)))
})

test_that("seqCpp the correct sequence of values", {
  expect_equal(seqCpp(from = 1, to = 10), matrix(1:10, ncol = 1))
  expect_equal(seqCpp(from = -5, to = 5), matrix(-5:5, ncol = 1))
})

test_that("seqCpp result can be transformed to a numeric vector", {
  expect_equal(c(seqCpp(from = 1, to = 10)), 1:10)
})

### Test 'rseqCpp'

test_that("rseqCpp returns a 1-row-matrix", {
  expect_equal(dim(rseqCpp(from = 1, to = 10)), dim(matrix(1:10, nrow = 1)))
})

test_that("rseqCpp the correct sequence of values", {
  expect_equal(rseqCpp(from = 1, to = 10), matrix(1:10, nrow = 1))
  expect_equal(rseqCpp(from = -5, to = 5), matrix(-5:5, nrow = 1))
})

test_that("rseqCpp result can be transformed to a numeric vector", {
  expect_equal(c(rseqCpp(from = 1, to = 10)), 1:10)
})

### Test 'factorialCpp'

test_that("factorialCpp works for 0 and 1", {
  expect_equal(factorialCpp(0), 1)
  expect_equal(factorialCpp(1), 1)  
})

test_that("factorialCpp works for other higher numbers", {
  expect_equal(factorialCpp(2), factorial(2))
  expect_equal(factorialCpp(3), factorial(3)) 
  expect_equal(factorialCpp(4), factorial(4))
  expect_equal(factorialCpp(5), factorial(5)) 
  expect_equal(factorialCpp(6), factorial(6))  
})