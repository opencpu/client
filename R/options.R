#' Client options
#'
#' Global [options][curl::curl_options] applied to each request to OpenCPU.
#'
#' @export
#' @param ... named curl options passed to [curl::handle_setopt()]
#' @examples # Turn on verbose for all requests:
#' ocpu_options(verbose = TRUE)
#'
#' # Turn off again
#' ocpu_options(verbose = NULL)
ocpu_options <- local({
  OPTS <- list()
  supported <- names(curl::curl_options())
  function(...){
    args <- list(...)
    for(i in seq_along(args)){
      field <- names(args[i])
      value <- args[[i]]
      bail_if_not(length(field) && nchar(field), "Invalid name for option %d", i)
      bail_if_not(field %in% supported, "Unsupported curl option: %s", field)
      OPTS[[field]] <<- value
    }
    return(as.list(OPTS))
  }
})
