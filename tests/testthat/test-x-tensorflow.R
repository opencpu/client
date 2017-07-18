context("External: Tensorflow + Python")

test_that("Use read tensoflow works",{
  # Check if server has tensorflow
  req <- ocpu('/library/tensorflow/info', stop_on_error = FALSE)
  if(req$status != 200)
    skip("Skipping Tensorflow test, 'tensorflow' is not installed")

  code <- "library(tensorflow)
    sess = tf$Session()
    hello <- tf$constant('Hello, TensorFlow!')
    sess$run(hello)"

  output <- ocpu_post_multipart('/library/base/R/identity', list(x = I(code)))
  expect_equal(output, "Hello, TensorFlow!")
})
