context("post parsing")

test_that("Posting formats",{
  expect_length(ocpu_post_rds('/library/stats/R/rnorm', list(n = 4)), 4)
  expect_length(ocpu_post_json('/library/stats/R/rnorm', list(mean = 3, n = 5)), 5)
  expect_length(ocpu_post_encoded('/library/stats/R/rnorm', list(mean = 3, n = 6, sd = 100)), 6)
  expect_length(ocpu_post_multipart('/library/stats/R/rnorm', list(n = 7)), 7)
  expect_length(ocpu_post_pb('/library/stats/R/rnorm', list(sd = 99, n = 8, mean = 3)), 8)
})

test_that("Empty POST body",{
  expect_length(ocpu_post_rds('/library/base/R/Sys.time'), 1)
  expect_length(ocpu_post_json('/library/base/R/Sys.time'), 1)
  expect_length(ocpu_post_encoded('/library/base/R/Sys.time'), 1)
  expect_length(ocpu_post_multipart('/library/base/R/Sys.time'), 1)
  expect_length(ocpu_post_pb('/library/base/R/Sys.time'), 1)
})

test_that("Error message for missing args",{
  expect_error(ocpu_post_rds('/library/stats/R/rnorm'), 'argument "n" is missing')
  expect_error(ocpu_post_json('/library/stats/R/rnorm'), 'argument "n" is missing')
  expect_error(ocpu_post_encoded('/library/stats/R/rnorm'), 'argument "n" is missing')
  expect_error(ocpu_post_multipart('/library/stats/R/rnorm'), 'argument "n" is missing')
  expect_error(ocpu_post_pb('/library/stats/R/rnorm'), 'argument "n" is missing')
})
