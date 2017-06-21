bail <- function(...){
  stop(sprintf(...), call. = FALSE)
}

bail_if <- function(x, ...){
  if(isTRUE(as.logical(x)))
    bail(...)
}

bail_if_not <- function(x, ...){
  if(!isTRUE(as.logical(x)))
    bail(...)
}

url_path <- function(...){
  args <- c(...)
  args <- sub("^/", "", args)
  args <- sub("/$", "", args)
  do.call(file.path, c(as.list(args), fsep = "/"))
}

# Make rawToChar consistent on Unix and Windows
rawToChar <- function(x){
  out <- base::rawToChar(x)
  Encoding(out) <- 'UTF-8'
  out
}
