test_that("odds ratio 95% function works", {
  expect_equal(OR_95CI(0.5, 0.02, 0.05, 3), "1.649 (1.585, 1.715)")
})
