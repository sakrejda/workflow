county_file = census_download_us_county_addrfeat_file('26', '165', '2020', '/tmp/downloads/') |> 
  purrr::map(as.character) |> 
  purrr::keep(stringr::str_detect, '\\.shp$') |> 
  purrr::flatten_chr()

sf::st_read(county_file, query = "select FULLNAME, TLID from tl_2020_26163_addrfeat WHERE FULLNAME LIKE '%{street_name}%'")



