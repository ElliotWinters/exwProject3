test_that("correct return type", {
  test1 <- my_knn_cv(my_penguins_train, my_penguins_class, 3, 5)
  expect_type(test1, "list")
})
