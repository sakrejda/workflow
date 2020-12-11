
#' Host URL for GADM
#'
#' @param url alternative url to use, defaults to know location
#' @return url as string
#'
#' @export
gadm_url = function(url = "https://biogeo.ucdavis.edu") return(url)

#' Current GADM version
#'
#' @param major major version to use
#' @param minor minor version to use
#' @param type return format, 'dot' means '{major}.{minor}', 
#'   'concatenate' means '{major}{minor}', 'major' means '{major}',
#'   and 'minor' means '{minor}'
#' @return gadm version in requested format
#'
#'   @export
gadm_version = function(major = 3, minor = 6, type = 'dot') {
  if (type == 'dot') {
    version = paste0(major, '.', minor)
  } else if (type == 'concatenate') {
    version = paste0(major, minor)
  } else if (type == 'major') {
    version = major
  } else if (type == 'minor') {
    version = minor
  } else {
    stop("Not a known version type.")
  }
  return(version)
}

#' Path to gpkg files on GADM host
#'
#' @param version version of GADM used in relative path
#' @return relative path to GADM files
#'
#' @export
gadm_gpkg_path = function(version = gadm_version()) {
  path = url_path("data/gadm", version, "/gpkg")
  return(path)
}

#' GADM zip file name
#'
#' @param code country code for the file
#' @param version version of GADM for the file
#' @return zip file name
#'
#' @export
gadm_gpkg_zip = function(
  code = "USA",
  version = gadm_version(type = 'concatenate') 
) {
  path = url_path("gadm", version, "_", code, "_gpkg.zip")
  return(path)
}

#' GADM zip file relative path
#'
#' @param code country code for the file
#' @param version version of GADM for the file
#' @return zip file name
#'
#' @export
gadm_gpkg_relative_path = function(
  code = "USA", 
  version = gadm_version()
) {
  path = gadm_gpkg_path(version)
  full_path = url_path(path, gadm_gpkg_zip(code, version))
  return(full_path)
}

#' GADM zip file full URL
#'
#' @param code country code for the file
#' @param version version of GADM for the file
#' @return zip file URL
#'
#' @export
gadm_gpkg_file_url = function(
  code = "USA", 
  version = gadm_version(),
  host = gadm_url()
) {
  full_url = url_path(host, gadm_gpkg_relative_path(code, version))
  return(full_url)
}

#' GADM zip file full file path
#'
#' @param code country code for the file
#' @param version version of GADM for the file
#' @return zip file full path
#'
#' @export
gadm_gpkg_file_path = function(
  code = "USA", 
  version = gadm_version(),
  data_dir = data_dir()
) {
  full_path = file.path(data_dir, gadm_gpkg_relative_path(code, version))
  return(full_path)
}

#' Check if a file matches the known GADM file name format
#'
#' @param path path to file to check
#' @param type file format of GADM file
#' @param zipped TRUE iff the file is zipped
#' @return TRUE iff the file matches specifications
#'
#' @export
is_gadm_file = function(path, type = 'gpkg', zipped = TRUE) {
  file_name = basename(paths)
  pattern =  paste0('gadm[0-9][0-9]_[A-Z][A-Z][A-Z]_', type)
  if (zipped) {
    pattern = paste0(pattern, '.zip')
  }
  matches = stringr::str_starts(string = file_name, pattern = pattern)
  return(matches)
}



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

