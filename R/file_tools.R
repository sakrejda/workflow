
#' Trim off last component of file name after a period
#'
#' No I don't want to always remember the stringr call
#'
#' @param path path to trim extension from
#' @return trimmed path
#'
#' @export
trim_extension = function(path) stringr::str_replace(path, pattern = '\\.[^.]+$', replacement = '')

#' Check if a path is usable
#'
#' This can be fiddly, follow the same rules:
#'
#' @param path path to check
#' @return TRUE iff path is usable
#'
#' @export
path_is_good = function(path) {
  if (is.null(path)) {
    stop(msg[['path-is-null']])
  }
  if (path == "") {
    stop(msg[['path-is-empty']])
  }
  if (length(path) != 1) {
    stop(msg[['path-is-vector']])
  }
  return(TRUE)
}

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
resource_dir = function(path, name) { 
  if (missing(path)) {
    stop(msg[['resource-missing-path']])
  }
  if (missing(name)) {
    stop(msg[['resource-missing-name']])
  }
  path_is_good(path)
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

#' Unzip a .zip file in the directory hosting the .zip file
#'
#' @param path path to file to unzip
#' @return paths of extracted files
#'
#' @export
unzip_file = function(path) {
  target_dir = trim_extension(path)
  o = unzip(path, exdir = target_dir)
  return(o)
}


#' Search for files under a root directory
#'
#' @param path root directory to search under
#' @param name pattern to search for
#' @param extension extension to search for 
#' @return full paths to all files
#'
#' @export
find_files = function(path, name, extension) {
  if (missing(name)) {
    name = ""
  }
  if (missing(extension)) {
    extension = ""
  } else {
    extension = paste0("\\.", extension, "$")
  }
  pattern = paste0(name, extension)
  files = dir(path = path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  return(files)
}
