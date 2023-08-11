
#' Default name standardizer for columns
#'
#' @param x colummn name
#' @export
#' @return standardized column name
standardize_name = tolower

#' Create a column definition from a list of arguments
#'
#' @param list of arguments to `ColumnMetadata` initializer
#' @return ColumnMetadata object
#' @export
make_column_definition = function(x) { 
  definition = purrr::lift_dl(ColumnMetadata$new)(x)
  return(definition)
}

#' Create a column definition from arbitrary arguments
#'
#' @param ... arguments to `ColumnMetadata` initializer
#' @return ColumnMetadata object
#' @export
column = function(...) {
  definition = rlang::exec(ColumnMetadata$new, ...)
  return(definition)
}

#' Combine multiple definitions for the same column 
#'
#' @param x list of definitions
#' @return list of definitions with one entry for each current name
#' @export
combine_definitions = function(x) {
  if (length(x) <= 1) {
    return(x)
  }
  cn = purrr::map_chr(x, ~ .x$name)
  sn = purrr::map_chr(x, ~ .x$standard_name)
  combined = paste(cn, sn, sep = ' -> ') |> duplicated() |>
    any() |> isFALSE()
  if (combined) {
    return(x)
  }
  n = length(cn) # min: 2
  done = integer()
  for (i in 1:(n-1)) {
    for (j in (i + 1):n) {
      if (j %in% done) {
        next
      } else if (cn[i] == cn[j] && sn[i] == sn[j]) {
        x[[i]]$merge(x[[j]], priority = 'other')
        done = c(done, j)
      }
    }
  }
  x = x[-done]
  return(x)
}


