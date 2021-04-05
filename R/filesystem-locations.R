
#' Assure that a resource directory for the named resource exists
#'
#' This is fiddly, follow the same rules 
#'   1) create if needed
#'   2) don't clobber
#'   3) fail if a file is where a directory should be
#'   4) give a clear message of the issues on failure
#'
#' @param path location to of resource
#' @param name name of environmental variable storing resource location
#' @return path of known-to-exist resource
#'
#' @export
resource_dir = function(name, path, ...) { 
  path = fs::path(path, ...)
  if (missing(path)) {
    stop(msg[['resource-missing-path']])
  }
  if (missing(name)) {
    stop(msg[['resource-missing-name']])
  }
  is_usable_path(path)
  path = fs::as_fs_path(path)
  if (fs::file_exists(path)) {
    if (fs::is_file(path)) {
      stop(msg[['resource-dir-is-file']])
    }
    if (!fs::is_dir(path)) {
      stop(msg[['resource-dir-is-not-dir']])
    }
  } else {
    fs::dir_create(path = path, recurse = TRUE)
    if (!fs::is_dir(path)) {
      stop(msg[['resource-dir-creation-failed']])
    }
  }
  set_env(name, path)
  return(path)
}

#' Retrieve the named resource
#'
#' Here a resource is a named location on a filesystem, can be
#' passed to fs::path, etc..., similar to here::here but doesn't do
#' a complex search and allows out-of-tree references.
#'
#' @param name name of the resource
#' @param ... additional relative path elements to resource
#' @param .root, optionally the new path root
#' @return path to resource, if available
#' 
#' @export
resource = function(name, ..., .root = NULL) {
  var = default_dir_var(name)
  val = get_env(var)
  if (is.null(val) || val == "") { # $VAR is not defined
      stop(msg[['resource-dir-var-empty']])
  }
  if (is.null(.root)) {
    path = val # using found path
  } else {
    path = .root
    set_env(var, path)
  }
  path = resource_dir(name = var, path = path, ...)
  return(path)
}

#' Retrieve a URI or directory relative to a named resource
#'
#' @param ... relative URI/path components
#' @param .name required (specified first and named) name or resource
#' @param strict iff FALSE (TRUE by default), failure to find the named resource downgrades to
#'   a temporary filesystem location.
#'
#' @export
get_dir = function(..., .name = NULL, .strict = TRUE) {
  if (is.null(.name)) {
    stop(msg[['resource-missing-name']])
  }
  path = try(resource(name = .name, ...))
  if ('try-error' %in% class(path)) {
    if (isTRUE(.strict)) {
      stop(msg[['resource-not-found-error']])
    } else {
      root = fs::path_temp(.name, ...)
      warning(msg[['resource-not-found']])
    }
  }
  return(path)
}

#' @export 
project_dir = function(...) get_dir(.name = "project", ...)

#' @export
build_dir = function(...) get_dir(.name = "build", ...)

#' @export
artifact_dir = function(...) get_dir(.name = "artifact", ...)

#' @export
data_dir = function(...) get_dir(.name = "data", ...)

#' @export
config_dir = function(...) get_dir(.name = "config", ...)

#' @export
cache_dir = function(...) get_dir(.name = "cache", ...)

