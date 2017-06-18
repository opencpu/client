#' OpenCPU API
#'
#' Calls an R function or script via the OpenCPU protocol and return output.
#'
#' @name ocpu
#' @rdname ocpu
#' @export
#' @importFrom curl new_handle handle_setheaders curl_fetch_memory parse_headers_list
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom protolite serialize_pb unserialize_pb
#' @param path target api (without the `/ocpu` part)
#' @param handle a curl handle to pass custom options and headers
#' @param server base URL to OpenCPU
ocpu <- function(path, handle = NULL, server = 'https://cloud.opencpu.org/ocpu'){
  url <- url_path(server, path)
  ocpu_perform(url, handle = handle)
}

ocpu_perform <- function(url, handle = NULL, stop_on_error = TRUE, no_cache = TRUE){
  if(!length(handle))
    handle <- new_handle()
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
#' @examples # Note server might send cached responses
#' ocpu_post_pb('/library/stats/R/rnorm', list(n = 5, mean = 3))
ocpu_post_pb <- function(path, args = NULL){
  handle <- new_handle(postfields = protolite::serialize_pb(args))
  handle_setheaders(handle, 'Content-Type' = 'application/rprotobuf')
  req <- ocpu(path, handle)
  bail_if(!length(req$location), "response did not contain a location!")
  req2 <- ocpu_perform(url_path(req$location, "R", ".val", "pb"))
  unserialize_pb(req2$content)
}

#' @export
#' @rdname ocpu
#' @examples ocpu_post_json('/library/stats/R/rnorm', list(n = 5, mean = 3))
ocpu_post_json <- function(path, args = NULL){
  handle <- new_handle(postfields = jsonlite::toJSON(args))
  handle_setheaders(handle, 'Content-Type' = 'application/json')
  req <- ocpu(path, handle)
  bail_if(!length(req$location), "response did not contain a location!")
  req2 <- ocpu_perform(url_path(req$location, "R", ".val", "json"))
  fromJSON(rawToChar(req2$content))
}
