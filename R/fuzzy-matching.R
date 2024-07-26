

#' A function to retrieve the top n candidate matches
#'
#' @param x vector of strings to match against candidates
#' @param candidates vector of strings that entries of x may match
#' @param n number of matches to keep
#' @return list of plausible matches
#'
#' @export
top_n_matches = function(x, candidates, n = 5) {
  require(stringdist)
  if (length(x) > 1) {
    return(purrr::map(x, top_n_matches, candidates, n))
  }
  distances = stringdist(x, candidates, method = 'jw')
  names(distances) = paste0("match_", 1:n, "_distance")
  top_n = candidates[order(distances)[1:n]]
  distances = distances[order(distances)[1:n]]
  names(top_n) = paste0("match_", 1:n)
  return(tibble::tibble_row(original = x, matches = list(top_n), distances = list(distances)))
}
