

#' Check if a path is usable
#'
#' This can be fiddly, follow the same rules:
#'
#' @param path path to check
#' @return TRUE iff path is usable
#'
#' @export
is_usable_path = function(path) {
  if (is.null(path)) {
    stop(msg[['path-is-null']])
  }
  if (is.na(path)) {
    stop(msg[['path-is-unknown']])
  }
  if (path == "") {
    stop(msg[['path-is-empty']])
  }
  if (length(path) != 1) {
    stop(msg[['path-is-vector']])
  }
  return(TRUE)
}

