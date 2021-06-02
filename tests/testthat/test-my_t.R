test_that("two.sided works", {
  test1 <- my_t.test(my_gapminder$lifeExp, "two.sided", 60)

  test1_exp <- t.test(my_gapminder$lifeExp, alternative = "two.sided", mu = 60)
  test1_expFormatted <- list(-1.679548, 1703, "two.sided", 0.0932)
  names(test1_expFormatted) <- c("test_stat", "df",
                           "alternative", "p_val")

  expect_equal(test1, test1_expFormatted)
})

test_that("less works", {
  test2 <- my_t.test(my_gapminder$lifeExp, "less", 60)
  test2_exp <- t.test(my_gapminder$lifeExp, alternative = "less", mu = 60)
  test2_expFormatted <- list(-1.679548, 1703, "less", 0.0466)
  names(test2_expFormatted) <- c("test_stat", "df",
                                 "alternative", "p_val")
  expect_equal(test2, test2_expFormatted)
})

test_that("greater works", {
  test3 <- my_t.test(my_gapminder$lifeExp, "greater", 60)
  test3_exp <- t.test(my_gapminder$lifeExp, alternative = "greater", mu = 60)
  test3_expFormatted <- list(-1.679548, 1703, "greater", 0.9534)
  names(test3_expFormatted) <- c("test_stat", "df",
                                 "alternative", "p_val")
  expect_equal(test3, test3_expFormatted)
})
