context("graphics")

test_that("Posting formats",{
  handle <- new_handle(copypostfields = 'x=rnorm(1e4)')
  req <- ocpu("/library/graphics/R/hist", handle = handle)
  expect_equal(req$status, 201)
  expect_match(req$location, req$session)
  svgdata <- ocpu_perform(url_path(req$location, "graphics", "1", "svg"))
  expect_equal(svgdata$status, 200)
  expect_equal(svgdata$type, "image/svg+xml")
  pngdata <- ocpu_perform(url_path(req$location, "graphics", "1", "png"))
  expect_equal(pngdata$status, 200)
  expect_equal(pngdata$type, "image/png")
  jpegdata <- ocpu_perform(url_path(req$location, "graphics", "1", "pdf"))
  expect_equal(jpegdata$status, 200)
  expect_equal(jpegdata$type, "application/pdf")

  # Test the 'last' alias
  svgdata <- ocpu_perform(url_path(req$location, "graphics", "last", "svg"))
  expect_equal(svgdata$status, 200)
  expect_equal(svgdata$type, "image/svg+xml")

  # Error when invalid image
  expect_error(ocpu_perform(url_path(req$location, "graphics", "2", "svg")), "not found")
  expect_error(ocpu_perform(url_path(req$location, "graphics", "0", "svg")), "not found")


})
