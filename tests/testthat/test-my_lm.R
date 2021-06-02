
library(dplyr)

test_that("linear formula works", {
  lm1_formula <- lifeExp ~ gdpPercap + continent
  test1 <- my_lm(lm1_formula, data = my_gapminder)
  test1_exp <- summary(lm(lm1_formula, my_gapminder))[[4]]
  expect_equal(test1, test1_exp, tolerance = 0.01)
})

test_that("marginal/interaction formula works", {
  test2 <- my_lm(lifeExp ~ gdpPercap*continent, data = my_gapminder)
  test2_exp <- summary(lm(lifeExp ~ gdpPercap*continent, my_gapminder))[[4]]
  expect_equal(test2, test2_exp, tolerance = 0.01)
})

