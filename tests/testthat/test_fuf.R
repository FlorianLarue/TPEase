source("../../R/fuf.R")

test_that("cmerge.all", {

  dfOne <- data.frame()
  dfTwo <- data.frame(a = c(1,2,3), b = c("A","B","C"))
  dfThree <- data.frame(c = c(TRUE, NA))
  df <- cmerge.all(dfOne,dfTwo,dfThree)

  expect_that(nrow(df), equals(3))
  expect_that(ncol(df), equals(3))
})
