test_that("myncurve works as expected", {
  resList = myncurve(10, 4, 10)
  expect_equal(resList$mu, 10)
  expect_equal(resList$sigma, 4)
  expect_equal(resList$probability, 0.5)
})
