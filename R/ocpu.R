#' OpenCPU API
#'
#' Calls an R function or script via the OpenCPU protocol and return output.
#'
#' @name ocpu
#' @rdname ocpu
#' @export
#' @importFrom curl new_handle handle_setheaders handle_setform handle_setopt
#' curl_fetch_memory parse_headers_list
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom protolite serialize_pb unserialize_pb
#' @param path target api (without the `/ocpu` part)
#' @param handle a curl handle to pass custom options and headers
#' @param server base URL to OpenCPU server
ocpu <- function(path, handle = new_handle(), server = ocpu_server(), stop_on_error = TRUE){
  url <- url_path(server, path)
  ocpu_perform(url, handle = handle, stop_on_error = stop_on_error)
}

#' @export
#' @rdname ocpu
ocpu_server <- local({
  SERVER <- Sys.getenv("OCPU_SERVER", "https://cloud.opencpu.org/ocpu")
  #SERVER <- Sys.getenv("OCPU_SERVER", "http://172.16.141.128:5656/ocpu")
  #SERVER <- Sys.getenv("OCPU_SERVER", "http://localhost:5656/ocpu")
  #SERVER <- Sys.getenv("OCPU_SERVER", "http://localhost/ocpu")
  function(server = NULL){
    if(length(server)){
      bail_if_not(grepl("https?://", server), "Does not look like URL: %s", server)
      SERVER <<- server
    }
    return(SERVER)
  }
})

ocpu_perform <- function(url, handle = new_handle(), stop_on_error = TRUE){
  handle <- handle_setopt(handle, .list = ocpu_options())
  req <- curl_fetch_memory(url, handle = handle)
  bail_if(req$status >= 400 && stop_on_error,
          "HTTP %d: %s",req$status, rawToChar(req$content))
  headers <- parse_headers_list(req$headers)
  list(
    url = req$url,
    status = req$status_code,
    session = headers[["x-ocpu-session"]],
    type = headers[["content-type"]],
    location = headers$location,
    content = req$content
  )
}

#' @export
#' @rdname ocpu
#' @param args a named list with function arguments, c.f. [do.call()]
#' @examples # Note that server might send cached responses
#' ocpu_post_json('/library/stats/R/rnorm', list(n = 5, mean = 3))
ocpu_post_json <- function(path, args = NULL){
  handle <- new_handle(copypostfields = jsonlite::toJSON(args))
  handle_setheaders(handle, 'Content-Type' = 'application/json')
  req <- ocpu(path, handle)
  bail_if(!length(req$location), "response did not contain a location!")
  req2 <- ocpu_perform(url_path(req$location, "R", ".val", "json"))
  fromJSON(rawToChar(req2$content))
}

#' @export
#' @rdname ocpu
#' @examples ocpu_post_encoded('/library/stats/R/rnorm', list(n = 5, mean = 3))
ocpu_post_encoded <- function(path, args = NULL){
  fields <- names(args)
  values <- vapply(args, function(x){
    if(!inherits(x, "AsIs"))
      x <- deparse_atomic(x)
    curl::curl_escape(x)
  }, character(1))
  data <- paste(fields, values, sep = "=", collapse = "&")
  handle <- new_handle(copypostfields = data)
  req <- ocpu(path, handle)
  bail_if(!length(req$location), "response did not contain a location!")
  req2 <- ocpu_perform(url_path(req$location, "R", ".val", "rds"))
  readRDS(gzcon(rawConnection(req2$content)))
}

#' @export
#' @rdname ocpu
#' @examples ocpu_post_multipart('/library/stats/R/rnorm', list(n = 5, mean = 3))
ocpu_post_multipart <- function(path, args = NULL){
  values <- lapply(args, function(x){
    if(inherits(x, c("form_file", "form_data", "AsIs")))
      return(x)
    if(is.atomic(x))
      return(deparse_atomic(x)) # curl forms automatically escape
    curl::form_data(serialize(x, NULL), "application/rds")
  })
  handle <- handle_setform(new_handle(), .list = values)
  req <- ocpu(path, handle)
  bail_if(!length(req$location), "response did not contain a location!")
  req2 <- ocpu_perform(url_path(req$location, "R", ".val", "rds"))
  readRDS(gzcon(rawConnection(req2$content)))
}

#' @export
#' @rdname ocpu
#' @examples ocpu_post_pb('/library/stats/R/rnorm', list(n = 5, mean = 3))
ocpu_post_pb <- function(path, args = NULL){
  handle <- new_handle(copypostfields = protolite::serialize_pb(args))
  handle_setheaders(handle, 'Content-Type' = 'application/rprotobuf')
  req <- ocpu(path, handle)
  bail_if(!length(req$location), "response did not contain a location!")
  req2 <- ocpu_perform(url_path(req$location, "R", ".val", "pb"))
  unserialize_pb(req2$content)
}

#' @export
#' @rdname ocpu
#' @examples ocpu_post_rds('/library/stats/R/rnorm', list(n = 5, mean = 3))
ocpu_post_rds <- function(path, args = NULL){
  handle <- new_handle(copypostfields = serialize(args, NULL))
  handle_setheaders(handle, 'Content-Type' = 'application/rds')
  req <- ocpu(path, handle)
  bail_if(!length(req$location), "response did not contain a location!")
  req2 <- ocpu_perform(url_path(req$location, "R", ".val", "rds"))
  readRDS(gzcon(rawConnection(req2$content)))
}

deparse_atomic <- function(x){
  if(is.character(x)){
    str <- jsonlite::toJSON(enc2utf8(x))
    str <- sub("^\\[", "c(", str)
    sub("\\]$", ")", str)
  } else if(is.atomic(x)) {
    paste(deparse(x), collapse = "\n")
  } else {
    unclass(jsonlite::toJSON(x, digits = NA))
  }
}
