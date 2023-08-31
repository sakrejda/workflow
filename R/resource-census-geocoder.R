
#' Endpoint for the geocoder
#'
#' Returns the census geocoder endpoint
#'
#' @param default the actual endpoint, should someone want to change it (?)
#' @return endpoint url
#'
#' @export
census_geocoder_api_endpoint = function(default = "https://geocoding.geo.census.gov/geocoder") default

#' Retrieve census geocoder benchmarks
#'
#' Benchmarks are the version of the data used to geocode
#'
#' @param endpoint base url for the service
#' @return list with all benchmarks descriptors
#'
#' @export
census_geocoder_api_benchmarks = function(
  endpoint = census_geocoder_api_endpoint()
) {
  url = glue::glue(endpoint, "/", "benchmarks")
  response = httr::GET(url)
  contents = httr::content(response, as = "parsed")[[1]]
  contents
}

#' Retrieve version ("vintage") for a given benchmark
#'
#' Versions within versions...
#'
#' @param endpoint base url for the service
#' @param benchmark the benchmark whose vintage we want
#' @return list with all vintage descriptors
#'
#' @export
census_geocoder_api_vintage = function(
  endpoint = census_geocoder_api_endpoint(),
  benchmark = census_geocoder_api_benchmarks() |> purrr::keep(~ .x$isDefault) |> purrr::flatten()
) {
  url = glue::glue(endpoint, "/", "vintages", "?", "benchmark={benchmark_id}", benchmark_id = benchmark$id)
  response = httr::GET(url)
  contents = httr::content(response, as = "parsed")
  contents
}

#' Internal function: checks that the geocoding target contains the correct components
#'
#' Checks target components and derives the search type
#'
#' @param target target to check
#' @return string indicating searchtype to use with the API
#'
#' @export
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
      all(c('street', 'city', 'state', 'zip') %in% components) ||
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

#' Function for doing a single API call for a single target
#'
#' @param target list with components for a valid call.
#' @param endpoint service endpoint
#' @param returntype either "locations" for basic API call or "geographies" if the
#'          call should also return which geographic entities the address exists within.
#' @param benchmark, as returned by census_geocoder_api_benchmarks
#' @param vintage, as returned by census_geocoder_api_vintage
#' @return a list gently re-formatted from the API call return, including
#'           status, any error messages, and the outcome of geocoding.
#'
#' @export
census_geocoder_api_call = function(
  target,
  endpoint = census_geocoder_api_endpoint(),
  returntype = "locations", # or "geographies"
  benchmark = census_geocoder_api_benchmarks() |> purrr::keep(~ .x$isDefault) |> purrr::flatten(), 
  vintage = census_geocoder_api_vintage()$vintages  |> purrr::keep(~ .x$isDefault) |> purrr::flatten(),
  cache_dir = workflow::build_dir("census-geocoder-cache")
) {
  target = target |> purrr::discard(is.na)
  hash = rlang::hash(c(target, endpoint, returntype, benchmark, vintage))
  cache_file = fs::path(cache_dir, hash)
  cache_exists = fs::file_exists(cache_file)
  if (cache_exists) {
      return(qs::qread(cache_file))
  }
  searchtype = census_geocoder_api_check_target_components(target)
  url_root = glue::glue("{endpoint}/{returntype}/{searchtype}?benchmark={benchmark_id}&vintage={vintage_id}",
    endpoint = endpoint,
    benchmark_id = benchmark$id, vintage_id = vintage$id,
    returntype = returntype, searchtype = searchtype)
  if (searchtype == 'onelineaddress') {
    url = glue::glue("{url_root}&address={onelineaddress}", 
      url_root = url_root,
      onelineaddress = stringr::str_replace_all(target$onelineaddress, ' ', '+'))
  } else if (searchtype == 'coordinates') {
    if (returntype != 'geographies') {
      rlang::abort("For a 'coordinates' searchtype the return type *must* be 'geographies'.")
    }
    url = glue::glue("{url_root}&x={x}&y={y}", url_root = url_root, x = target$x, y = target$y)
  } else if (searchtype == 'address') {
    url = glue::glue("{url_root}&street={street}", 
      url_root = url_root, 
      street = stringr::str_replace_all(target$street, ' ', '+'))
    if ('city' %in% names(target)) {
      url = glue::glue("{url}&city={city}", url = url, city = stringr::str_replace_all(target$city, ' ', '+'))
    }
    if ('state' %in% names(target)) {
      url = glue::glue("{url}&state={state}", url = url, state = stringr::str_replace_all(target$state, ' ', '+'))
    }
    if ('zip' %in% names(target)) {
      url = glue::glue("{url}&zip={zip}", url = url, zip = stringr::str_replace_all(target$zip, ' ', '+'))
    }
  } else {
    rlang::abort("Internal error: 'searchtype' parameter not calculated properly.")
  }
  response = httr::GET(url)
  status = httr::http_status(response)
  result = list()
  result$status = status
  result$code = httr::status_code(response)
  result$url = url
  result$target = target
  if (isTRUE(status$category == "Success")) {  
    result$response = httr::content(response, as = "parsed")
  } else {
    result$response = list()
  }
  if (isTRUE(status$category == "Success") && 
      isTRUE(length(result$response$result$addressMatches) > 0)
  ) {
    result$cache_file = cache_file
    result$cached = TRUE
    qs::qsave(result, file = cache_file)
  } else {
    result$cache_file = ""
    result$cached = FALSE
  }
  result
}

#' A function to flatten the heavily nested API output slightly
#'
#' An opinionated flattening of the API output
#'
#' @param x output from census_geocoder_api_call function
#' @return flatter list with a tibble containing the results
#'
#' @export
census_geocoder_flatten_result = function(x) {
    r = x$response$result
    if (length(r$addressMatches) > 0) {
          address_tibble = r$addressMatches[[1]]$addressComponents
          address_tibble$side = r$addressMatches[[1]]$tigerLine$side
          address_tibble$tiger_line_id = r$addressMatches[[1]]$tigerLine$tigerLineId
          address_tibble$longitude = r$addressMatches[[1]]$coordinates$x
          address_tibble$latitude = r$addressMatches[[1]]$coordinates$y
          address_tibble$one_line_address = r$addressMatches[[1]]$matchedAddress
          address_tibble = tibble::as_tibble(address_tibble) |>
            dplyr::mutate(street_name = streetName, 
                pre_qualifier = preQualifier, pre_type = preType, pre_direction = preDirection,
                suffix_direction = suffixDirection, from_address = fromAddress, to_address = toAddress,
                suffix_qualifier = suffixQualifier, suffix_type = suffixType, zip_code = zip) |>
            dplyr::select(tiger_line_id, from_address, to_address, side, 
                pre_qualifier, pre_direction, pre_type, street_name, 
                suffix_type, suffix_direction, suffix_qualifier,
                city, state, zip_code, one_line_address, 
                longitude, latitude)
    } else {
        address_tibble = tibble::tibble(
            tiger_line_id = NA_character_, from_address = NA_character_, to_address = NA_character_, side = NA_character_,
            pre_qualifier = NA_character_, pre_direction = NA_character_, pre_type = NA_character_,
            street_name = NA_character_, 
            suffix_type = NA_character_, suffix_direction = NA_character_, suffix_qualifier = NA_character_,
            city = NA_character_, state = NA_character_, zip_code = NA_character_, one_line_address = NA_character_,
            longitude = NA_real_, latitude = NA_real_)
    }
    
    x$address_tibble = address_tibble
    return(x)
}

#' Function to geocode a batch (data frame) of addresses in street/city/state/zip format
#'
#' @param batch data frame containing columns with parts of the address
#' @param street bare symbol for street column
#' @param city bare symbol for city column
#' @param state bare symbol for state column
#' @param zip bare symbol for zip code column
#' @param cache_file optional path to file to use to cache results to avoid submitting them
#'   multiple times. Can be shared between calls or it will be based on a hash of the 
#'   batch input data.
#' @return list of geocoded components and resulting tables with some simplified output
#'
#' @export
census_geocoder_batch = function(
    batch, 
    street = street, 
    city = city, 
    state = state, 
    zip = zip,
    cache_dir = workflow::build_dir("census-geocoder-cache"),
    ...
) {
  street = rlang::enquo(street)
  city = rlang::enquo(city)
  state = rlang::enquo(state)
  zip = rlang::enquo(zip)
  
  batch = batch |> 
    dplyr::transmute(
        batch_row = 1:dplyr::n(),
        row_hash = purrr::map_chr(paste0(!!street, !!city, !!state, !!zip), rlang::hash),
        street = !!street, city = !!city, state = !!state, zip = !!zip)
                
  responses = batch |>
    dplyr::select(-batch_row) |>
    unique() |>
    dplyr::group_split(row_hash) |>
    purrr::map(census_geocoder_api_call, cache_dir = cache_dir, ...) |>
    purrr::map(census_geocoder_flatten_result)
      
  coding = batch |>
    dplyr::select(batch_row, row_hash) |>
    dplyr::left_join(
        y = purrr::map(responses, ~ dplyr::bind_cols(row_hash = .x$target[['row_hash']], .x$address_tibble)) |> dplyr::bind_rows(),
        by = 'row_hash')
  
  list(batch = batch, coding = coding, responses = responses)
}


#' Function to geocode a large (data frame) of addresses in street/city/state/zip format
#'
#' The data here must still be able to fit in memory easily, data duplication does
#' occur. 
#'
#' @param batch data frame containing columns with parts of the address
#' @param street bare symbol for street column
#' @param city bare symbol for city column
#' @param state bare symbol for state column
#' @param zip bare symbol for zip code column
#' @param cache_dir optional path to file to use to cache results to avoid submitting them
#'   multiple times. Can be shared between calls or it will be based on a hash of the 
#'   batch input data.
#' @param batch_size how many records should be sent to a worker at one time
#' @param n_processes how many futures workers should there be
#' @param ... other arguments to the batch geocoder
#' @return list of geocoded components and resulting tables with some simplified output
#'
#' @export
census_geocoder_multi_batch = function(
    data, 
    street = street, 
    city = city, 
    state = state, 
    zip = zip,
    strategy = future::multisession,
    strategy_args = list(workers = parallel::detectCores()/2, rscript_libs = .libPaths()),
    cache_dir = workflow::build_dir("census-geocoder-cache"),
    batch_size = 100,
    n_processes = 10,
    ...
) {
    n_processes;
    street = rlang::enquo(street)
    city = rlang::enquo(city)
    state = rlang::enquo(state)
    zip = rlang::enquo(zip)
    extra_args = list(...) 
    old_plan = future::plan(strategy, !!!strategy_args)
    on.exit({future::plan(old_plan)}, add = TRUE)
    data = data |>
      dplyr::group_split(batch_id = 0:(dplyr::n() - 1) %/% batch_size)
    coded = list()
    for (i in seq_along(data)) {
      coded[[i]] = promises::future_promise(expr = {
          census_geocoder_batch(data[[i]], !!street, !!city, !!state, !!zip, cache_dir, !!!extra_args)
        }, packages = "workflow")$then(
            onFulfilled = function(x) return(x),
            onRejected = function(x) return(x))
    }
    finalized = rep(FALSE, length(coded))
    while(!all(finalized)) {
      #print(glue::glue("Completion: {progress}%\n", progress = round(mean(finalized)*100)))
      for (i in seq_along(finalized)) {
          finalized[i] = isTRUE(environment(coded[[i]]$then)$private$state != "pending")
      }
      Sys.sleep(0.1); later::run_now()
    }
    o = list()
    for (i in seq_along(coded)) {
      o[[i]] = environment(coded[[i]]$then)$private$value
    }
    return(o)
}
