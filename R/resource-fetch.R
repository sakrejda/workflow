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
#' @return paths to downloaded files
#'
#' @export
fetch = function(urls, paths) {
  require(curl)
  data_dirs = dirname(paths) |> unique()
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
  paths = gadm_gpkg_file_path(code, version, data_dir)
  fetch(urls, paths)
  files = find_files(path = data_dir, name = 'USA', extension = 'gpkg')
  return(files)
}

#' Fetch USCB US block files
#'
#' @param year year version to fetch
#' @param type either empty string (default) or 'edges' to select between the two availble.
#' @param format spatial data format, 'gdb' by default
#' @param host host to fetch from (defaults)
#' @param data_dir directory to place files under 
#' @return list of downloaded files written successfully 
#'
#' @export
fetch_uscb_tiger_us_gdb = function(
  year, 
  type = "block",
  format = "gdb",
  host = uscb_api_endpoint(), 
  data_dir = workflow::data_dir()
) {
  urls = url_path(host, uscb_tiger_us_relative_path(year, type, format))
  paths = file.path(data_dir, uscb_tiger_us_relative_path(year, type, format))
  fetch(urls, paths)
  files = find_files(path = data_dir, name = paste0('tlgdb_', year), extension = 'gpkg')
  return(files)
}

#' Fetch USCB state spatial database
#'
#' @param year year version to fetch
#' @param state two-letter code for state abbreviation to cover
#' @param type either empty string (default) or 'edges' to select between the two availble.
#' @param host host to fetch from (defaults)
#' @param data_dir directory to place files under 
#' @return list of downloaded files written successfully 
#'
#' @export
fetch_uscb_tiger_state_gdb = function(
  year,
  state,
  type = '',
  format = 'gdb',
  host = uscb_api_endpoint(), 
  data_dir = workflow::data_dir()
) {
  urls = url_path(hist, uscb_tiger_us_relative_path(year, state, type, format))
  paths = file.path(data_dir, uscb_tiger_us_relative_path(year, state, type, format))
  fetch(urls, paths)
  files = find_files(path = data_dir, name = paste0('tlgdb_', year), extension = 'gpkg')
  return(files)
}