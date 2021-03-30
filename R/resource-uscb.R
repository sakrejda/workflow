
#' Host URL for USCB data
#'
#' @param url alternative url to use, defaults to know location
#' @return url as string
#'
#' @export
uscb_url = function(url = "https://www2.census.gov") return(url)

#' Resource path munging based on characteristics
#'
#' @param year in four digits
#' @param format currently only 'gdb'
#' @param location currently only 'tiger' (default)
#' @return relative path to the resource
#'
#' @export
uscb_path = function(
  year = 2019,
  format = 'gdb', 
  location = 'tiger'
) {
  if (nchar(year) != 4) {
    stop("Requires 4-digit year.")
  }
  if (location == "tiger") {
    path_stub = "geo/tiger"
    if (format == 'gdb') {
      yr = substr(year, 3, 4)
      path_stub = url_path(path_stub, paste0('TGRGDB', yr))
    }
  } else {
    stop("USCB resource location unknown.")
  }
  return(path_stub)
}

#' File name munging from characteristics
#'
#' @param year in four digits
#' @param area area the file covers
#' @param type spatial data type (block by default)
#' @param format currently only 'gdb' (default)
#' @param location currently only 'tiger' (default)
#' @return file name for the resource
#'
#' @export
uscb_file = function(
  year = 2019,
  area = 'us',
  type = 'block',
  format = 'gdb',
  location = 'tiger'
) {
  if (nchar(year) != 4) {
    stop("Require 4-digit year.")
  }
  if (location  == "tiger") {
    stub = paste0('tlgdb_', year, "_a_", area, "_", type, ".", format, ".zip")
  } else {
    stop("USCB resource location unknown.")
  }
  return(stub)
}

#' Relative path to USCB data
#'
#' @param year in four digits
#' @param area area the file covers
#' @param type spatial data type (block by default)
#' @param format currently only 'gdb' (default)
#' @param location currently only 'tiger' (default)
#' @return relative file path name for the resource
#'
#' @export
uscb_relative_path = function(
  year = 2019,
  area = "us",
  type = "block",
  format = 'gdb',
  location = 'tiger'
) {
  path_stub = uscb_path(year, format, location)
  file_stub = uscb_file(year, area, type, format, location)
  path = url_path(path_stub, file_stub)
  return(path)
}

#' Relative path to USCB block spatial data
#'
#' @param year in four digits
#' @param area area the file covers
#' @param format currently only 'gdb' (default)
#' @param location currently only 'tiger' (default)
#' @return relative file path name for the resource
#'
#' @export
uscb_block_relative_path = function(year = 2019, ...) {
  p = uscb_relative_path(year = year, type = 'block', ...)
  return(p)
}

#' Relative path to USCB block zip code spatial data
#'
#' @param year in four digits
#' @param area area the file covers
#' @param format currently only 'gdb' (default)
#' @param location currently only 'tiger' (default)
#' @return relative file path name for the resource
#'
#' @export
uscb_zip_relative_path = function(year = 2019, ...) {
  p = uscb_relative_path(year = year, type = 'addr', ...)
  return(p)
}


#' USCB block gdb zip file full URL
#'
#' @return zip file URL
#'
#' @export
uscb_block_file_url = function(
  year = 2019,
  ...,
  host = uscb_url()
) {
  full_url = url_path(host, uscb_block_relative_path(year, ...))
  return(full_url)
}

#' USCB zip code gdb zip full file path
#'
#' @return zip file full path
#'
#' @export
uscb_gpkg_file_path = function(
  year = 2019,
  ...,
  data_dir = data_dir()
) {
  full_path = file.path(data_dir, uscb_zip_relative_path(year, ...))
  return(full_path)
}

