context("concurrency")

test_that("concurrent requests", {
  pool <- curl::new_pool()
  url <- url_path(ocpu_server(), "library", "base", "R", "sqrt", "pb")
  numbers <- runif(5)
  values <- c()
  sessions <- c()
  lapply(numbers, function(num){
    handle <- curl::new_handle(url = url, copypostfields = paste0('x=', num))
    curl::handle_setopt(handle, .list = ocpu_options())
    curl::multi_add(handle, pool = pool, done = function(res){
      expect_equal(res$status, 201)
      headers <- curl::parse_headers_list(res$headers)
      data <- protolite::unserialize_pb(res$content)
      values <<- c(values, data)
      sessions <<- c(sessions, headers[["x-ocpu-session"]])
    })
  })
  expect_length(curl::multi_list(pool), 5)
  results <- curl::multi_run(pool = pool)
  expect_equal(sum(sqrt(numbers)), sum(values))

  # Compute sum on server
  args <- stats::setNames(lapply(sessions, I), letters[seq_along(sessions)])
  res <- ocpu_post_encoded("library/base/R/sum", args)
  expect_equal(res, sum(values))

  # Using namespace notation
  args <- stats::setNames(lapply(paste0(sessions, "::.val"), I), letters[seq_along(sessions)])
  res <- ocpu_post_encoded("library/base/R/sum", args)
  expect_equal(res, sum(values))

  # Using dotted notation
  args <- list("..." = I(paste(sessions, collapse = ",")))
  res <- ocpu_post_encoded("library/base/R/sum", args)
  expect_equal(res, sum(values))
})
