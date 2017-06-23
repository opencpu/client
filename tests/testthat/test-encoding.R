context("encodings")

test_that("Server UTF-8 support", {
  json <- base::rawToChar(ocpu('/library/opencpu/data/strings/json')$content)
  Encoding(json) <- 'UTF-8'
  data <- jsonlite::fromJSON(json)
  len <- vapply(data, nchar, numeric(1), USE.NAMES = FALSE)
  expect_equal(len, c(6, 5, 6, 2, 40, 17))

  # Compare to original
  strings <- opencpu::strings
  expect_identical(strings, data)

  # Objects do not get deparsed and end up in the call
  obj <- structure(as.list(strings), names = letters[seq_along(strings)])

  # Roundtrip string objects
  res <- ocpu_post_multipart('/library/base/R/identity', list(x = obj))
  expect_identical(res, obj)
  res <- ocpu_post_encoded('/library/base/R/identity', list(x = obj))
  expect_identical(res, obj)
  res <- ocpu_post_json('/library/base/R/identity', list(x = obj))
  expect_identical(res, obj)
  res <- ocpu_post_rds('/library/base/R/identity', list(x = obj))
  expect_identical(res, obj)
  res <- ocpu_post_pb('/library/base/R/identity', list(x = obj))
  expect_identical(res, obj)

  # Get version of evaluate
  evaluate_version <- ocpu_post_pb('/library/utils/R/packageVersion', list(pkg = 'evaluate'))
  webutils_version <- ocpu_post_pb('/library/utils/R/packageVersion', list(pkg = 'webutils'))

  # Test all native strings (usualy only on Windows)
  skip_if_not(evaluate_version >= "0.10.1" && webutils_version >= "0.6")
  expect_equal(ocpu_post_multipart('/library/base/R/list', obj), obj)
  expect_equal(ocpu_post_encoded('/library/base/R/list', obj), obj)
  expect_equal(ocpu_post_json('/library/base/R/list', obj), obj)
  expect_equal(ocpu_post_pb('/library/base/R/list', obj), obj)

  # Do not serialize non-utf8 on Windows
  obj_utf8 <- structure(as.list(enc2utf8(strings)), names = letters[seq_along(strings)])
  expect_equal(ocpu_post_rds('/library/base/R/list', obj_utf8), obj_utf8)


})
