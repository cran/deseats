test_that("Filter transformation works when 'ar' and 'ma' are 'numeric(0)'", {
  expect_equal(arma_to_ar(max_i = 4), c(-1, 0, 0, 0, 0))
  expect_equal(arma_to_ma(max_i = 4), c(1, 0, 0, 0, 0))
  expect_true(all(-arma_to_ar(max_i = 100) == arma_to_ma(max_i = 100)))
})

test_that("Filter transformation works when either only 'ar' or 'ma' is of length 1", {
  expect_equal(arma_to_ar(ar = 0.5, max_i = 4), c(-1, 0.5, 0, 0, 0))
  expect_equal(arma_to_ar(ma = 0.5, max_i = 4), c(-1, 0.5, -0.25, 0.125, -0.0625)) 
  expect_equal(arma_to_ma(ar = 0.5, max_i = 4), c(1, 0.5, 0.25, 0.125, 0.0625))
  expect_equal(arma_to_ma(ma = 0.5, max_i = 4), c(1, 0.5, 0, 0, 0)) 
})

test_that("Filter transformation works in other common cases", {
  # ar and ma of length one and usage of different signs
  expect_equal(arma_to_ar(ar = 0.5, ma = 0.3, max_i = 4), c(-1, 0.8, -0.24, 0.072, -0.0216))
  expect_equal(arma_to_ar(ar = -0.5, ma = 0.3, max_i = 4), c(-1, -0.2, 0.06, -0.018, 0.0054))
  expect_equal(arma_to_ar(ar = 0.5, ma = -0.3, max_i = 4), c(-1, 0.2, 0.06, 0.018, 0.0054))
  expect_equal(arma_to_ar(ar = -0.5, ma = -0.3, max_i = 4), c(-1, -0.8, -0.24, -0.072, -0.0216))  
  
  expect_equal(arma_to_ma(ar = 0.5, ma = 0.3, max_i = 4), c(1, 0.8, 0.4, 0.2, 0.1))
  expect_equal(arma_to_ma(ar = -0.5, ma = 0.3, max_i = 4), c(1, -0.2, 0.1, -0.05, 0.025))
  expect_equal(arma_to_ma(ar = 0.5, ma = -0.3, max_i = 4), c(1, 0.2, 0.1, 0.05, 0.025))
  expect_equal(arma_to_ma(ar = -0.5, ma = -0.3, max_i = 4), c(1, -0.8, 0.4, -0.2, 0.1))  
  
  # ar of length 2 and ma of length 1 and different signs on ma
  expect_equal(arma_to_ar(ar = c(1.2, -0.4), ma = 0.3, max_i = 4), c(-1, 1.5, -0.85, 0.255, -0.0765))
  expect_equal(arma_to_ar(ar = c(1.2, -0.4), ma = -0.3, max_i = 4), c(-1, 0.9, -0.13, -0.039, -0.0117))
    
  expect_equal(arma_to_ma(ar = c(1.2, -0.4), ma = 0.3, max_i = 4), c(1, 1.5, 1.4, 1.080, 0.736))
  expect_equal(arma_to_ma(ar = c(1.2, -0.4), ma = -0.3, max_i = 4), c(1, 0.9, 0.68, 0.456, 0.2752))  
  
  # ma of length 2 and ar of length 1 and different signs on ar
  expect_equal(arma_to_ar(ar = 0.3, ma = c(1.2, 0.4), max_i = 4), c(-1, 1.5, -1.4, 1.08, -0.736))
  expect_equal(arma_to_ar(ar = -0.3, ma = c(1.2, 0.4), max_i = 4), c(-1, 0.9, -0.68, 0.456, -0.2752))
    
  expect_equal(arma_to_ma(ar = 0.3, ma = c(1.2, 0.4), max_i = 4), c(1, 1.5, 0.85, 0.255, 0.0765))
  expect_equal(arma_to_ma(ar = -0.3, ma = c(1.2, 0.4), max_i = 4), c(1, 0.9, 0.13, -0.039, 0.0117))  
    
  
})

test_that("Test filter functions for incorrect input", {
  expect_error(arma_to_ar(ar = "Hello"))
  expect_error(arma_to_ar(ma = "Hello"))
  expect_error(arma_to_ar(max_i = "Hello"))
  expect_error(arma_to_ar(max_i = c(4, 5)))  
  
  expect_error(arma_to_ma(ar = "Hello"))
  expect_error(arma_to_ma(ma = "Hello"))
  expect_error(arma_to_ma(max_i = "Hello"))
  expect_error(arma_to_ma(max_i = c(4, 5)))    
})