context("encodings")

test_that("Server UTF-8 support", {
  json <- base::rawToChar(ocpu('/library/opencpu/data/strings/json')$content)
  Encoding(json) <- 'UTF-8'
  data <- jsonlite::fromJSON(json)
  len <- vapply(data, nchar, numeric(1), USE.NAMES = FALSE)
  expect_equal(len, c(6, 5, 6, 2, 40, 17))

  # Compare to original
  strings <- unserialize(ocpu('/library/opencpu/data/strings/rds')$content)
  expect_identical(strings, data)

  # Also test latin1 on Windows
  strings[1] <- enc2native(strings[1])

  # Objects do not get deparsed and end up in the call
  obj <- structure(as.list(strings), names = letters[seq_along(strings)])

  # Roundtrip atomic
  expect_identical(ocpu_post_multipart('/library/base/R/identity', list(x = strings)), strings)
  expect_identical(ocpu_post_encoded('/library/base/R/identity', list(x = strings)), strings)
  expect_identical(ocpu_post_json('/library/base/R/identity', list(x = strings)), strings)
  expect_identical(ocpu_post_rds('/library/base/R/identity', list(x = strings)), strings)
  expect_identical(ocpu_post_pb('/library/base/R/identity', list(x = strings)), strings)

  # Roundtrip objects
  expect_identical(ocpu_post_multipart('/library/base/R/identity', list(x = obj)), obj)
  expect_identical(ocpu_post_encoded('/library/base/R/identity', list(x = obj)), obj)
  expect_identical(ocpu_post_json('/library/base/R/identity', list(x = obj)), obj)
  expect_identical(ocpu_post_rds('/library/base/R/identity', list(x = obj)), obj)
  expect_identical(ocpu_post_pb('/library/base/R/identity', list(x = obj)), obj)

  # Test all native strings (usualy only on Windows)
  expect_equal(ocpu_post_multipart('/library/base/R/list', obj), obj)
  expect_equal(ocpu_post_encoded('/library/base/R/list', obj), obj)
  expect_equal(ocpu_post_json('/library/base/R/list', obj), obj)
  expect_equal(ocpu_post_rds('/library/base/R/list', obj), obj)
  expect_equal(ocpu_post_pb('/library/base/R/list', obj), obj)

})
