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
  buf <- serialize(mtcars, NULL)
  out <- ocpu_post_multipart('/library/base/R/list',
                             list(x = curl::form_data(buf, "application/rds"), y = "foobar"))
  expect_identical(out$x, mtcars)
  expect_identical(out$y, "foobar")

})
