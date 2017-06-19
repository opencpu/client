context("encodings")

test_that("String encodings", {
  strings_utf8 <- c(
    "Zürich",
    "北京填鴨们",
    "ผัดไทย",
    "寿司",
    rawToChar(as.raw(1:40)),
    "?foo&bar=baz!bla\n"
  )
  Encoding(strings_utf8) <- "UTF-8"
  obj_utf8 <- structure(as.list(strings_utf8), names = letters[seq_along(strings_utf8)])
  obj_native <- structure(as.list(enc2native(strings_utf8)), names = letters[seq_along(strings_utf8)])

  # Test all UTF-8 strings
  expect_equal(ocpu_post_multipart('/library/base/R/list', obj_utf8), obj_utf8)
  expect_equal(ocpu_post_encoded('/library/base/R/list', obj_utf8), obj_utf8)
  expect_equal(ocpu_post_json('/library/base/R/list', obj_utf8), obj_utf8)
  expect_equal(ocpu_post_rds('/library/base/R/list', obj_utf8), obj_utf8)
  expect_equal(ocpu_post_pb('/library/base/R/list', obj_utf8), obj_utf8)

  # Test all native strings (usualy only on Windows)
  expect_equal(ocpu_post_multipart('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_encoded('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_json('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_rds('/library/base/R/list', obj_native), obj_utf8)
  expect_equal(ocpu_post_pb('/library/base/R/list', obj_native), obj_utf8)
})
