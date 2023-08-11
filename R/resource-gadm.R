

#' Host URL for GADM
#'
#' @param url alternative url to use, defaults to know location
#' @return url as string
#'
#' @export
gadm_url = function(url = "https://geodata.ucdavis.edu") return(url)

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
gadm_version = function(major = 4, minor = 1, type = 'dot') {
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
  path = url_path("gadm", paste0("gadm", version), "gpkg")
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
  path = paste0("gadm", version, "_", code, ".gpkg")
  return(path)
}

#https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_USA.gpkg


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
  version = stringr::str_replace(version, '\\.', '')
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
  pattern =  paste0('gadm[0-9][0-9]_[A-Z][A-Z][A-Z]', type)
  if (zipped) {
    pattern = paste0(pattern, '.zip')
  }
  matches = stringr::str_starts(string = file_name, pattern = pattern)
  return(matches)
}



