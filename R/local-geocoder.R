

#' Retrieve the ADDRFEAT file for a given place/time
#'
#' This function is fragile, expects very specific arguments.
#'
#' @param state state geo_id as a string of two numbers
#' @param county county geo_id as a string of three numbers
#' @param year vintage year of requested file as a string of four numbers
#' @return path to local cache location of the downlaoded unzipped files
#'
#' @export
census_download_us_county_addrfeat_file = function(
  state,
  county,
  year = "2020",
  cache_dir = workflow::build_dir("census-geocoder-addrfeat-cache")
) {
  url_host = "https://www2.census.gov/"
  url_path = "geo/tiger/TIGER{year}/ADDRFEAT/"
  url_file = "tl_{year}_{state}{county}_addrfeat.zip"
  url_template = paste0(url_host, url_path, url_file)
  url = glue::glue(url_template, year = year, state = state, county = county)
  local_file = glue::glue("address-features--state-{state}--county-{county}--year-{year}.zip", 
    year = year, state = state, county = county)
  local_path = fs::path(cache_dir, local_file)
  workflow::fetch(url, local_path)
  workflow::unzip_file(local_path)
  files_original = fs::dir_ls(path = fs::path_ext_remove(local_path), 
    regexp = glue::glue('tl_{year}_{state}{county}_addrfeat\\..*$',
    year = year, state = state, county = county))
  extensions = purrr::map_chr(files_original, stringr::str_extract, '\\..[a-z.]*')
  files_new = fs::path(fs::path_ext_remove(local_path), 
    purrr::map_chr(extensions, 
      ~ glue::glue('address_features__state_{state}__county_{county}__year_{year}{ext}',
      year = year, state = state, county = county, ext = .x)))
  purrr::map2(files_original, files_new, fs::file_move)
  return(files_new)
}



#' Retrieve the COUSUB file for a given place/time
#'
#' This function is fragile, expects very specific arguments.
#'
#' @param state state geo_id as a string of two numbers
#' @param year vintage year of requested file as a string of four numbers
#' @return path to local cache location of the downlaoded unzipped files
#'
#' @export
census_download_us_state_cousub_file = function(
  state,
  year = "2020",
  cache_dir = workflow::build_dir("census-geocoder-addrfeat-cache")
) {
  url_host = "https://www2.census.gov/"
  url_path = "geo/tiger/TIGER{year}/COUSUB/"
  url_file = "tl_{year}_{state}_cousub.zip"
  url_template = paste0(url_host, url_path, url_file)
  url = glue::glue(url_template, year = year, state = state)
  local_file = glue::glue("county-subdivisions--state-{state}--year-{year}.zip", 
    year = year, state = state)
  local_path = fs::path(cache_dir, local_file)
  workflow::fetch(url, local_path)
  workflow::unzip_file(local_path)
  files_original = fs::dir_ls(path = fs::path_ext_remove(local_path), 
    regexp = glue::glue('tl_{year}_{state}_cousub\\..*$',
    year = year, state = state))
  extensions = purrr::map_chr(files_original, stringr::str_extract, '\\..[a-z.]*')
  files_new = fs::path(fs::path_ext_remove(local_path), 
    purrr::map_chr(extensions, 
      ~ glue::glue('county_subdivisions__state_{state}__year_{year}{ext}',
      year = year, state = state, ext = .x)))
  purrr::map2(files_original, files_new, fs::file_move)
  return(files_new)
}

#' Retrieve the PLACE file for a given place/time
#'
#' This function is fragile, expects very specific arguments.
#'
#' @param state state geo_id as a string of two numbers
#' @param year vintage year of requested file as a string of four numbers
#' @return path to local cache location of the downlaoded unzipped files
#'
#' @export
census_download_us_state_place_file = function(
  state,
  year = "2020",
  cache_dir = workflow::build_dir("census-geocoder-addrfeat-cache")
) {
  url_host = "https://www2.census.gov/"
  url_path = "geo/tiger/TIGER{year}/PLACE/"
  url_file = "tl_{year}_{state}_place.zip"
  url_template = paste0(url_host, url_path, url_file)
  url = glue::glue(url_template, year = year, state = state)
  local_file = glue::glue("place-data--state-{state}--year-{year}.zip", 
    year = year, state = state)
  local_path = fs::path(cache_dir, local_file)
  workflow::fetch(url, local_path)
  workflow::unzip_file(local_path)
  files_original = fs::dir_ls(path = fs::path_ext_remove(local_path), 
    regexp = glue::glue('tl_{year}_{state}_place\\..*$',
    year = year, state = state))
  extensions = purrr::map_chr(files_original, stringr::str_extract, '\\..[a-z.]*')
  files_new = fs::path(fs::path_ext_remove(local_path), 
    purrr::map_chr(extensions, 
      ~ glue::glue('place_data__state_{state}__year_{year}{ext}',
      year = year, state = state, ext = .x)))
  purrr::map2(files_original, files_new, fs::file_move)
  return(files_new)
}

#' Geocode a local dataset on the given columns and reference year for spatial data
#'
#' @param data data frame to pull address columns from
#' @param street unquoted address column name for street address component
#' @param city unquoted city column component of the address
#' @param state unquoted state column component of the address
#' @param zip unquoted zip code column component of the address
#' @param year four-digit year string for spatial data reference year
#' @return data frame with the original colums as well as the geocoded location tract
#'
#' @export
census_local_tract_geocoder = function(
  data,
  street = street,
  city = city,
  state = state,
  county = county,
  zip = zip,
  reference_year = "2020"
) {
  
    street = rlang::enquo(street)
    city = rlang::enquo(city)
    state = rlang::enquo(state)
    zip = rlang::enquo(zip)
    county = rlang::enquo(county)

    data = data |> dplyr::group_split(!!county)
    batch_state = data |> purrr::map(dplyr::pull, !!state) |> unique()
    batch_county = data |> purrr::map(dplyr::pull, !!county) |> unique()
    for (i in seq_along(data)) {
      county_files = census_download_us_county_addrfeat_file(batch_state, batch_county, reference_year)
      county_data = count_files |> 
        purrr::keep(stringr::str_detect, '\\.shp$') |>
        sf::st_read()
      county_zip_codes = c(county_data$ZIPL, county_data$ZIPR) |>
        unique() |> sort()
      county_street_names = unique(county_data$FULLNAME)
      batch_street_v = dplyr::pull(data[[i]], !!street)
      batch_city_v = dplyr::pull(data[[i]], !!city)
      batch_zip_v = dplyr::pull(data[[i]], !!zip)
      for (j in 1:nrow(data[[i]])) {
        if (batch_zip_v[j] %in% county_zip_codes) {
          idx_zip_match_l = stringr::str_which(county_data$ZIPL, batch_zip_v[j])
          idx_zip_match_r = stringr::str_which(county_data$ZIPR, batch_zip_v[j])
          idx_zip_candidate_rows = c(idx_zip_match_l, idx_izp_match_r) |> 
            unique() |> sort()
        } else {
          idx_zip_candidate_rows = NULL
        }
        if (batch_street_v[j] %in% county_street_names) {
          idx_street_match = stringr::str_which(county_data$FULLNAME, batch_street_v[j])
        } else {}
      }
    }
    return(data)
 }
