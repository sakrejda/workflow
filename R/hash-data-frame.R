hash_df = function(df) {
  s = df |> purrr::map(as.character) |>
    purrr::flatten_chr() |>
    paste(collapse = '')
  h = openssl::sha512(s)
  return(h)
}


