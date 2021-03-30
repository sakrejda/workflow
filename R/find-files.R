

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
