context("Testing get_data2")

xlsx <- system.file("exdata", "resources.xlsx", package="recon")

test_that("get_data2 gets all sheets", {
  
  d <- get_data2(xlsx)
  expect_true( is.list(d))
})




context("Testing make_net2")

test_that("make_data2 works at all", {
  d <- get_data2(xlsx)
  g <- make_net2(d)
})