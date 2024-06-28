
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

census_local_geocoder = function(
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
      county_file = census_download_us_county_addrfeat_file(batch_state, batch_county, reference_year)
      batch_street_v = dplyr::pull(data[[i]], !!street)
      batch_city_v = dplyr::pull(data[[i]], !!city)
      batch_zip_v = dplyr::pull(data[[i]], !!zip)
      for (j in 1:nrow(data[[i]])) {
        
      }
    }
 }
