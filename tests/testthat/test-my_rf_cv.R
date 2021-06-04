test_that("correct return type", {
  test1 <- my_rf_cv(5)
  expect_type(test1, "double")
})
