test_that("BV4.1 fitting functions return correct output", {
  expect_error(BV4.1(log(EXPENDITURES), type = 1), 'type muste be either NULL, "quarterly" or "monthly"')
  expect_error(BV4.1(log(EXPENDITURES), type = c("quarterly", "monthly")), 'type muste be either NULL, "quarterly" or "monthly"')  
  expect_error(BV4.1(c(log(EXPENDITURES)), type = NULL), 'The procedure cannot be applied to data with frequency 1.')  
  expect_no_error(BV4.1(c(log(EXPENDITURES)), type = "quarterly"))
})
