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
  obj_utf8 <- structure(as.list(enc2utf8(strings)), names = letters[seq_along(strings)])

  # Roundtrip string objects
  res <- ocpu_post_multipart('/library/base/R/identity', list(x = obj_utf8))
  expect_identical(res, obj_utf8)
  expect_identical(sapply(res, Encoding), sapply(obj_utf8, Encoding))
  res <- ocpu_post_multipart('/library/base/R/identity', list(x = obj_utf8))
  expect_identical(res, obj_utf8)
  expect_identical(sapply(res, Encoding), sapply(obj_utf8, Encoding))
  res <- ocpu_post_json('/library/base/R/identity', list(x = obj_utf8))
  expect_identical(res, obj_utf8)
  expect_identical(sapply(res, Encoding), sapply(obj_utf8, Encoding))
  res <- ocpu_post_rds('/library/base/R/identity', list(x = obj_utf8))
  expect_identical(res, obj_utf8)
  expect_identical(sapply(res, Encoding), sapply(obj_utf8, Encoding))
  res <- ocpu_post_pb('/library/base/R/identity', list(x = obj_utf8))
  expect_identical(res, obj_utf8)
  expect_identical(sapply(res, Encoding), sapply(obj_utf8, Encoding))

  # Strings as arguments (expressions)
  # enc <- Encoding(strings)
  # expect_identical(ocpu_post_multipart('/library/base/R/Encoding', list(x = strings)), enc)
  # expect_identical(ocpu_post_encoded('/library/base/R/Encoding', list(x = strings)), enc)
  # expect_identical(ocpu_post_json('/library/base/R/Encoding', list(x = strings)), enc)
  # expect_identical(ocpu_post_rds('/library/base/R/Encoding', list(x = strings)), enc)
  # expect_identical(ocpu_post_pb('/library/base/R/Encoding', list(x = strings)), enc)

  # Get version of evaluate
  evaluate_version <- ocpu_post_pb('/library/utils/R/packageVersion', list(pkg = 'evaluate'))

  # Test all native strings (usualy only on Windows)
  skip_if_not(evaluate_version >= "0.10.1")
  obj_native <- structure(as.list(enc2native(strings)), names = letters[seq_along(strings)])
  expect_equal(ocpu_post_multipart('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_encoded('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_json('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_rds('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_pb('/library/base/R/list', obj_native), obj_utf8)
})
