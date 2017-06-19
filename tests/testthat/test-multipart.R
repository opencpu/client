context("multipart")

test_that("Primitives and code", {
  # Quoting arguments in I() will post literal expressions
  x <- "sqrt(pi)"
  y <- "iris"
  z <- jsonlite::toJSON(mtcars)
  out1 <- ocpu_post_encoded('/library/base/R/list', list(x = I(x), y = I(y), z = I(z)))
  expect_identical(out1$x, sqrt(pi))
  expect_identical(out1$y, iris)
  expect_equal(out1$z, mtcars)

  # Otherwise they become strings
  out2 <- ocpu_post_encoded('/library/base/R/list', list(x = x, y = y, z = z))
  expect_identical(out2$x, x)
  expect_identical(out2$y, y)
  expect_identical(out2$z, z)

  # Same for multipart
  out3 <- ocpu_post_multipart('/library/base/R/list', list(x = I(x), y = I(y), z = I(z)))
  expect_identical(out3$x, sqrt(pi))
  expect_identical(out3$y, iris)
  expect_equal(out3$z, mtcars)

  out4 <- ocpu_post_multipart('/library/base/R/list', list(x = x, y = y, z = z))
  expect_identical(out4$x, x)
  expect_identical(out4$y, y)
  expect_identical(out4$z, z)
})


test_that("File Uploads", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(iris, tmp, row.names = FALSE)
  out <- ocpu_post_multipart('/library/utils/R/read.csv', list(file = curl::form_file(tmp)))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(iris))
  out2 <- ocpu_post_multipart('/library/utils/R/read.csv', list(file = curl::form_file(tmp),
                                                                header = FALSE))
  expect_s3_class(out2, "data.frame")
  expect_equal(nrow(out2), nrow(iris)+1)
})

test_that("Data Uploads",{
  # Upload a RDS field
  x1 <- curl::form_data(serialize(mtcars, NULL), "application/rds")
  y <- "foobar"
  out1 <- ocpu_post_multipart('/library/base/R/list', list(x1 = x1, y = y))
  expect_identical(out1$x1, mtcars)
  expect_identical(out1$y, y)

  # Upload a JSON field
  x2 <- curl::form_data( jsonlite::toJSON(mtcars), "application/json")
  out2 <- ocpu_post_multipart('/library/base/R/list', list(x2 = x2, y = y))
  expect_equal(out2$x2, mtcars)
  expect_identical(out2$y, y)

  # Upload a ProtoBuf field
  x3 <- curl::form_data(protolite::serialize_pb(iris), "application/protobuf")
  out3 <- ocpu_post_multipart('/library/base/R/list', list(x3 = x3, y = y))
  expect_identical(out3$x3, iris)
  expect_identical(out3$y, y)

  # Upload raw binary data
  bindata <- serialize(rnorm(1e4), NULL)
  x4 <- curl::form_data(bindata, "application/octet-stream")
  out4 <- ocpu_post_multipart('/library/base/R/list', list(x4 = x4, y = y))
  expect_identical(out4$x4, bindata)
  expect_identical(out4$y, y)

  # Upload some text type
  textdata <- "I like cookies"
  x5 <- curl::form_data(textdata, "text/plain")
  out5 <- ocpu_post_multipart('/library/base/R/list', list(x5 = x5, y = y))
  expect_identical(out5$x5, textdata)
  expect_identical(out5$y, y)

  # All together now :D
  out <- ocpu_post_multipart('/library/base/R/list', list(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5))
  expect_equal(out$x1, mtcars)
  expect_equal(out$x2, mtcars)
  expect_equal(out$x3, iris)
  expect_equal(out$x4, bindata)
  expect_equal(out$x5, textdata)
})

test_that("Multipart errors", {
  expect_error(ocpu_post_multipart('/library/base/R/list', list(x = I("doesnotexit()"))), "doesnotexit")
  expect_error(ocpu_post_multipart('/library/base/R/list', list(x = I("[1,4,]}"))), "pars")
  expect_error(ocpu_post_multipart('/library/base/R/list', list(x = curl::form_data("foo", "application/blabla"))), "blabla")
})
