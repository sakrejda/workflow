
#' Unzip a .zip file
#'
#' @param path path to file to unzip
#' @param target where to root unzipped file
#' @return paths of extracted files
#'
#' @export
unzip_file = function(path, target = fs::path_dir(path)) {
  if (missing(target)) {
    target = fs::path_ext_remove(path)
  }
  o = unzip(path, exdir = target)
  return(o)
}

