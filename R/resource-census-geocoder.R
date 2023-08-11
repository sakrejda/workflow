census_geocoder_api_endpoint = function(default = "https://geocoding.geo.census.gov/geocoder") default

census_geocoder_api_benchmarks = function(
  endpoint = census_geocoder_api_endpoint()
) {
  url = glue::glue(endpoint, "/", "benchmarks")
  response = httr::GET(url)
  contents = httr::content(response, as = "parsed")[[1]]
  contents
}

census_geocoder_api_vintage = function(
  endpoint = census_geocoder_api_endpoint(),
  benchmark = census_geocoder_api_benchmarks() |> purrr::keep(~ .x$isDefault) |> purrr::flatten()
) {
  url = glue::glue(endpoint, "/", "vintages", "?", "benchmark={benchmark_id}", benchmark_id = benchmark$id)
  response = httr::GET(url)
  contents = httr::content(response, as = "parsed")
  contents
}

census_geocoder_api_check_target_components = function(target) {
  components = names(target)
  if (length(components) == 0) {
    msg = glue::glue("Target is zero-length, no target has been specified.")
    rlang::abort(msg, failed_target = target)
  }
  type_is_onelineaddress = length(components) == 1 && components == 'onelineaddress'
  type_is_coordinates = length(components) == 2 && 'x' %in% components && 'y' %in% components
  type_is_address = length(components >=1) && length(components <= 4) &&
    ("street" %in% components) && (
      all(c('street', 'city', 'state') %in% components) ||
      all(c('street', 'zip') %in% components))
  if (type_is_onelineaddress) {
    return("onelineaddress")
  } else if (type_is_coordinates) {
    return("coordinates")
  } else if (type_is_address) {
    return("address")
  } else {
    rlang::abort("Geocoding target type falls outside the known categories", failed_target = target)
  }
  rlang::abort("Something went wrong in 'census_geocoder_api_check_target_components' function, fell through the floor.")
}

census_geocoder_api_call = function(
  target,
  endpoint = census_geocoder_api_endpoint(),
  returntype = "locations", # or "geographies"
  #searchtype = "onelineaddress", # or "address" or "coordinates"
  benchmark = census_geocoder_api_benchmarks() |> purrr::keep(~ .x$isDefault) |> purrr::flatten(), 
  vintage = census_geocoder_api_vintage()$vintages  |> purrr::keep(~ .x$isDefault) |> purrr::flatten()
) {
  searchtype = census_geocoder_api_check_target_components(target)
  url_root = glue::glue(endpoint, "/", {returntype}, "/", {searchtype}, "?", 
      "benchmark={benchmark_id}&vintage={vintage_id}",
    benchmark_id = benchmark$id, vintage_id = vintage$id,
    returntype = returntype, searchtype = searchtype)
  if (searchtype == 'onelineaddress') {
    url = glue::glue(url_root, "&", "address={onelineaddress}", 
      onelineaddress = stringr::str_replace_all(target$onelineaddress, ' ', '+'))
  } else if (searchtype == 'coordinates') {
    if (returntype != 'geographies') {
      rlang::abort("For a 'coordinates' searchtype the return type *must* be 'geographies'.")
    }
    url = glue::glue(url_root, "&", "x={x}&y={y}", x = target$x, y = target$y)
  } else if (searchtype == 'address') {
    url = glue::glue(url_root, "&", "street={street}", street = stringr::str_replace_all(target$street, ' ', '+'))
    if ('city' %in% names(target)) {
      url = glue::glue(url, "&", "city={city}", city = stringr::str_replace_all(target$city, ' ', '+'))
    }
    if ('state' %in% names(target)) {
      url = glue::glue(url, "&", "state={state}", state = stringr::str_replace_all(target$state, ' ', '+'))
    }
    if ('zip' %in% names(target)) {
      url = glue::glue(url, "&", "zip={zip}", zip = stringr::str_replace_all(target$zip, ' ', '+'))
    }
  } else {
    rlang::abort("Internal error: 'searchtype' parameter not calculated properly.")
  }
  cat(url)
  cat("\n")
  response = httr::GET(url)
  contents = httr::content(response, as = "parsed")
  contents
}


  