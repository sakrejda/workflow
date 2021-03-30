#' Like 'paste' but specificall for gluing URL's
#'
#' @param ... strings and values to compose into the URL
#' @return URL
#'
#' @export
url_path = function(...) {
  full_path = paste(..., sep = "/")
  url_path = stringr::str_replace(full_path, '/+', '/')
  return(url_path)
}

#' Fetch files from under a host and place files under a set of paths
#'
#' @param urls urls on host to fetch from
#' @param paths paths to place files in
#' @return 
#'
#' @export
fetch = function(urls, paths) {
  require(curl)
  data_dirs = dirname(paths) %>% unique()
  purrr::map(data_dirs, ~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))
  complete = file.exists(paths)
  urls = urls[!complete]
  paths = paths[!complete]
  pool = curl::new_pool()
  schedule = purrr::map2(urls, paths, ~ curl::curl_fetch_multi(url = .x, data = .y, pool = pool))
  outcome = curl::multi_run(pool = pool)
  return(paths)
}

#' Fetch GADM files
#'
#' @param code which country code to fetch for
#' @param version version to fetch (defaults)
#' @param host host to fetch from (defaults)
#' @param data_dir directory to place files under 
#' @return list of downloaded files written successfully 
#'
#' @export
fetch_gadm = function(code, version = gadm_version(), host = gadm_url(), data_dir = data_dir()) {
  urls = gadm_gpkg_file_url(code, version, host)
  paths = gadm_gpkg_file_path(code, version, host)
  files = find_files(path = data_dir, name = 'USA', extension = 'gpkg')
  return(files)
}

#' Fetch USCB block files
#'
#' @param version version to fetch (defaults)
#' @param host host to fetch from (defaults)
#' @param data_dir directory to place files under 
#' @return list of downloaded files written successfully 
#'
#' @export
fetch_uscb = function(code, version = uscb_version(), host = uscb_url(), data_dir = data_dir()) {
  urls = uscb_gpkg_file_url(code, version, host)
  paths = uscb_gpkg_file_path(code, version, host)
  files = find_files(path = data_dir, name = 'USA', extension = 'gpkg')
  return(files)
}

