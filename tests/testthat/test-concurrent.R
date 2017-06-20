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
      val <- protolite::unserialize_pb(res$content)
      expect_match(headers$location, headers[["x-ocpu-session"]])
      values <<- c(values, val)
      sessions <<- c(sessions, headers[["x-ocpu-session"]])

      # Recursively adds more jobs to the pool
      obj_handle <- curl::new_handle(url = url_path(headers$location, "R", ".val", "rds"))
      curl::multi_add(obj_handle, pool = pool, done = function(res){
        expect_equal(res$status, 200)
        expect_equal(val, unserialize(res$content))
      })
    })
  })
  expect_length(curl::multi_list(pool), 5)
  results <- curl::multi_run(pool = pool)
  expect_equal(results$success, 10)
  expect_equal(results$error, 0)
  expect_equal(results$pending, 0)

  # Test outcomes
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

# This checks that even if the request raises an error the session gets stored
test_that("concurrent errors",{
  pool <- curl::new_pool()
  url <- url_path(ocpu_server(), "library", "stats", "R", "runif", "pb")
  sessions <- c()
  lapply(1:5, function(i){
    handle <- curl::new_handle(url = url, customrequest = 'POST')
    curl::handle_setopt(handle, .list = ocpu_options())
    curl::multi_add(handle, pool = pool, done = function(res){
      expect_equal(res$status, 400)
      expect_match(rawToChar(res$content), "missing")
      headers <- curl::parse_headers_list(res$headers)
      sessions <<- c(sessions, headers[["x-ocpu-session"]])

      # Recursively adds more jobs to the pool
      obj_handle <- curl::new_handle(url = url_path(headers$location, "console"))
      curl::multi_add(obj_handle, pool = pool, done = function(res){
        expect_equal(res$status, 200)
        expect_match(rawToChar(res$content), "runif()")
        expect_match(rawToChar(res$content), "missing")
      })
    })
  })
  expect_length(curl::multi_list(pool), 5)
  results <- curl::multi_run(pool = pool)
  expect_equal(results$success, 10)
  expect_equal(results$error, 0)
  expect_equal(results$pending, 0)
})
