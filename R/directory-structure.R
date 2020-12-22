
#' Retrieve (weak-reference style), the named resource
#'
#' Here a resource is a named location on a filesystem, can be
#' passed to fs::path, etc..., similar to here::here but more generic
#'
#' @param name name of the resource
#' @param path, optionally the relative path above the resource
#' @return path to resource, if available
#' 
#' @export
resource = function(name, path = NULL) {
  var = default_dir_var(name)
  val = get_env(var)
  if (is.null(val) || val == "") { # $VAR is not defined
      stop(msg[['resource-dir-var-empty']])
  }
  if (is.null(path)) {
    path = val # using found path
  } 
  set_env(var, path)
  path = resource_dir(name = var, path = path)
  return(path)
}

#' Retrieve a URI or directory relative to a named resource
#'
#' FIXME: currently relies on `fs::path` so *only* works with directories
#'
#' @param ... relative URI/path components
#' @param name required (specified first and named) name or resource
#' @param strict iff FALSE (TRUE by default), failure to find the named resource downgrades to
#'   a temporary filesystem location.
#'
#' @export
get_dir = function(..., name = NULL, strict = TRUE) {
  if (is.null(name)) {
    stop(msg[['resource-missing-name']])
  }
  root = try(resource(name = name))
  if ('try-error' %in% class(root)) {
    if (strict) {
      stop(msg[['resource-not-found-error']])
    } else {
      root = fs::path_temp()
      warning(msg[['resource-not-found']])
    }
  }
  path = fs::path(root, ...)
  return(path)
}

#' @export
data_dir = function(...) get_dir(name = "data", ...)

#' @export
artifact_dir = function(...) get_dir(name = "artifact", ...)

#' @export
build_dir = function(...) get_dir(name = "build", ...)
