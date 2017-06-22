context("encodings")

test_that("String encodings", {
  strings <- c(
    "Zürich",
    "北京填鴨们",
    "ผัดไทย",
    "寿司",
    rawToChar(as.raw(1:40)),
    "?foo&bar=baz!bla\n"
  )
  obj <- structure(as.list(strings), names = letters[seq_along(strings)])

  # Roundtrip string objects
  res <- ocpu_post_multipart('/library/base/R/identity', list(x = obj))
  expect_identical(res, obj)
  res <- ocpu_post_multipart('/library/base/R/identity', list(x = obj))
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
