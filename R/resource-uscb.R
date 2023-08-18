
#' Host URL for USCB data
#'
#' @param url alternative url to use, defaults to know location
#' @return url as string
#'
#' @export
uscb_api_endpoint = function(url = "https://www2.census.gov") return(url)

#' Resource path munging based on characteristics
#'
#' @param year in four digits
#' @param format currently only 'gdb'
#' @return relative path to the resource
#'
#' @export
uscb_tiger_path = function(
  year = 2022,
  format = 'gdb'
) {
  if (nchar(year) != 4) {
    stop("Requires 4-digit year.")
  }
  path_stub = "geo/tiger"
  if (format == 'gdb') {
    yr = substr(year, 3, 4)
    path_stub = url_path(path_stub, paste0('TGRGDB', yr))
  } else {
    stop("USCB resource format unknown.")
  }
  return(path_stub)
}

#' File name munging from characteristics
#'
#' @param year in four digits
#' @param type spatial data type (block by default)
#' @param format currently only 'gdb' (default)
#' @return file name for the resource
#'
#' @export
uscb_tiger_us_file = function(
  year = 2022,
  type = 'block',
  format = 'gdb'
) {
  if (nchar(year) != 4) {
    stop("Require 4-digit year.")
  }
  stub = paste0('tlgdb_', year, "_a_", "us", "_", type, ".", format, ".zip")
  return(stub)
}

#' Relative path to USCB data
#'
#' @param year in four digits
#' @param type spatial data type (block by default)
#' @param format currently only 'gdb' (default)
#' @return relative file path name for the resource
#'
#' @export
uscb_tiger_us_relative_path = function(
  year = 2019,
  type = "block",
  format = "gdb"
) {
  path_stub = uscb_tiger_path(year, format)
  file_stub = uscb_tiger_us_file(year, type, format)
  path = url_path(path_stub, file_stub)
  return(path)
}


uscb_state_numbering = tibble::tibble(
  state = c("al", "ak", "az", "ar", "ca", "co", 
    "ct", "de", "dc", "fl", "ga", "hi", "id", "il", "in", "ia", "ks", 
    "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", 
    "nv", "nh", "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", 
    "ri", "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", 
    "wy", "as", "gu", "mp", "pr", "vi"), 
  number = c("01", "02", "04", "05", "06", "08", "09", "10", "11", 
    "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", 
    "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
    "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
    "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", 
    "55", "56", "60", "66", "69", "72", "78"))

#' File name munging from characteristics
#'
#' @param year in four digits
#' @param state two-letter code for state to include
#' @param type spatial data type (blank or "edges")
#' @param format currently only 'gdb' (default)
#' @return file name for the resource
#'
#' @export
uscb_tiger_state_file = function(
  year = 2022,
  state = "MI",
  type = '',
  format = 'gdb'
) {
  if (nchar(year) != 4) {
    stop("Require 4-digit year.")
  }
  state_lc = tolower(state)
  state_number = uscb_state_numbering |>
    dplyr::filter(state == state_lc) |>
    dplyr::pull(number)
  if (type == '') {
    file = paste0('tlgdb_', year, "_a_", state_number, "_", state_lc, ".", format, ".zip")
  } else {
    file = paste0('tlgdb_', year, "_a_", state_number, "_", state_lc, "_", type, ".", format, ".zip")
  }
  return(file)
}

#' Relative path to USCB state spatial data
#'
#' @param year in four digits
#' @param state two letter code for state to download
#' @param type spatial data type ('' is default, alternatively 'edges')
#' @param format currently only 'gdb' (default)
#' @return relative file path name for the resource
#'
#' @export
uscb_tiger_state_relative_path = function(
  year = 2019,
  state = 'MI',
  type = '',
  format = "gdb"
) {
  path_stub = uscb_tiger_path(year, format)
  file_stub = uscb_tiger_state_file(year, state, type, format)
  path = url_path(path_stub, file_stub)
  return(path)
}
