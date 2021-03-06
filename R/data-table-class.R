

simple_standardizer = function(s) s %>% tolower %>% stringr::str_replace_all('[ _]', '-')

#' A class representing a table of data based on a (remote?) file
#'
#' @description
#' Data...
#'
#  @details
#' FIXME: definitions should be viewable in their aggregate effect, not just
#' returned as an object.  Exported but probably used through helper functions.
#'
#' @exportClass DataTable
#' @export
DataTable = R6::R6Class(classname = "DataTable", 
  public = list(
    initialize = function(
      uri,
      rpath,
      retrieve = rdrop2::drop_download(path = .source_path, local_path = .local_path),
      load = haven::read_sas(data_file = .local_path),
      .data_dir = workflow::data_dir(),
      .build_dir = workflow::build_dir(),
      .artifact_dir = workflow::artifact_dir(),
      .binary_format = 'qs',
      ...
    ) {
      private$.update_path(uri, rpath, .data_dir, .build_dir, .binary_format)
      private$.update_artifact_dir(.artifact_dir)
      private$.update_build_dir(.build_dir)
      private$.retrieve = rlang::enquo(retrieve)
      private$.load = rlang::enquo(load)
      private$.update_cached_file(...)
      private$.load_cached()
      private$.colnames = colnames(private$.data)
      private$.save_local()
      private$.data = tibble::tibble()
      private$.dots = rlang::enquos(...)
    },
    contains = function(...) {
      symbols = rlang::enquos(...) %>% purrr::map(rlang::quo_text)
      check = all(symbols %in% self$colnames) %>% isTRUE()
      return(check)
    },
    define = function(...) {
      args = list(...)
      names = names(args)
      args_column_metadata = args %>% purrr::keep(~ isTRUE('ColumnMetadata' %in% class(.x)))
      args_other = args %>% 
        purrr::keep(~ !isTRUE('ColumnMetadata' %in% class(.x))) %>%
        purrr::imap(~ {if (is.null(.x$name)) .x$name = .y; .x}) %>% 
        purrr::map(make_column_definition)
      args = c(args_column_metadata, args_other)
      for (definition in args) {
        private$.insert_definition(definition)
      }
      return(self)
    },
    rename = function(...) {
      text = rlang::enquos(...) %>% purrr::map(rlang::quo_text) %>%
        purrr::imap(~ list(name = .x, standard_name = .y))
      purrr::lift_dl(self$define)(text)
      return(self)
    },
    mutate = function(...) {
      private$.load_local()
      original_colnames = private$.colnames
      private$.data = private$.data %>% dplyr::mutate(...)
      new_colnames = colnames(private$.data)
      if (length(original_colnames) != length(new_colnames) || 
          !all(original_colnames %in% new_colnames) ||
          !all(new_colnames %in% original_colnames)) {
        private$.colnames = colnames(private$.data)
      }
      private$.save_local()
    },
    process_definitions = function() {
      private$.definitions = combine_definitions(private$.definitions)
      private$.apply_definitions()  
      return(self)
    },
    attributes = function(k,v) {
      if (missing(k) && missing(v)) {
        return(private$.attributes)
      } else if (!missing(k) && missing(v)) {
        return(private$.attributes[[k]])
      } else if (!missing(k) && !missing(v)) {
        private$.attributes[[k]] = v
        return(private$.attributes[[k]])
      } else if (missing(k) && !missing(v)) {
        rlang::abort("Trying to set value of unknown attribute, attributes must be named.")
      }
      rlang::abort("Failed to set attribute, bad arguments.", object = self)
      return(self)
    },
    clear_definitions = function() {
      private$.definitions = list()
      return(self)
    },
    switch_disk_format = function(to = 'qs') {
      from = fs::path_ext(private$.local_binary_path)
      if (to == from) {
        return(TRUE)
      }
      private$.load_local()
      if (to == 'qs') {
        private$.local_binary_path = fs::path_ext_set(private$.local_binary_path, 'qs')
      } else {
        private$.local_binary_path = fs::path_ext_set(private$.local_binary_path, 'rds')
      }
      private$.save_local()
    }
  ),
  private = list(
    .definitions = list(),
    .retrieve = rlang::quo(),
    .load = rlang::quo(),
    .path_standardizer = simple_standardizer,
    .data = tibble::tibble(),
    .colnames = character(),
    .uri = character(),
    .rpath = character(),
    .source_path = fs::path(),
    .file_name = character(),
    .file_name_stub = character(),
    .file_name_ext = character(),
    .local_dir = fs::path(),
    .local_path = fs::path(),
    .local_binary_path = fs::path(),
    .dots = list(),
    .data_dir = character(),
    .artifact_dir = character(),
    .build_dir = character(),
    .logger = function(...) logger::log_info(...),
    .attributes = list(),
    .applied = integer(),
    .apply_definitions = function() {
      original_colnames = private$.colnames
      for (i in seq_along(private$.definitions)) {
        if (i %in% private$.applied) {
          next
        }
        name = private$.definitions[[i]]$name
        standard_name = private$.definitions[[i]]$standard_name
        if (!is.na(standard_name) && (name != standard_name)) {
          private$.logger("For file '{file_name}', renaming: {current} --> {standard}",
            file_name = private$.file_name, current = name, standard = standard_name)
          if (name %in% private$.colnames) {
            col_mask = (self$.colnames == name)
            private$.colnames = private$.colnames %>%
              purrr::map_if(~ isTRUE(.x == name), ~ standard_name, ~ .x) %>%
              purrr::flatten_chr()
            private$.applied = c(private$.applied, i)
          } else {
            msg = glue::glue("For file '{file_name}' renaming failed. The data does not contain",
              " a column named '{name}'.", file_name = private$.file_name, name = name)
            private$.logger(msg)
          }
        } else {
          private$.logger("For file '{file_name}, skipped renaming '{name}', ",
            "the current name is already standardized.", 
            file_name = private$.file_name, name = name)
        }
      }
      private$.logger("For file '{file_name}', all definitions applied.", 
        file_name = private$.file_name)
      if (any(original_colnames != private$.colnames)) {
        private$.load_local()
        colnames(private$.data) = private$.colnames
        private$.save_local()
      }
    },
    .insert_definition = function(x) {
      if (!(x$name %in% private$.colnames)) {
        private$.logger("Name '{name}' is not contained in the current data.", name = x$name)
      }
      n = length(private$.definitions)
      private$.definitions[[n+1]] = x
      if (is.null(x$standard_name)) {
        private$.logger("For file '{file_name}', inserted definition for column '{name}'.", 
          file_name = private$.file_name, name = x$name)
      } else {
        private$.logger("For file '{file_name}', inserted definition for column '{name}' as '{standard_name}'.", 
          file_name = private$.file_name, name = x$name, standard_name = x$standard_name)
      }
    },
    .load_local = function() {
      from = private$.local_binary_path
      if (!fs::file_exists(from)) {
        private$.load_cached(!!!private$.dots)
      }
      private$.logger("loading processed file from '{from}'.", from = from)
      if (fs::path_ext(from) == 'rds') {
        private$.data = readRDS(file = private$.local_binary_path)
      } else if (fs::path_ext(from) == 'qs') {
        private$.data = qs::qread(from)
      }
    },
    .load_cached = function(...) {
      private$.update_cached_file(!!!private$.dots)
      private$.logger("Loading file from: {local_path}", local_path = private$.local_path)
      local_data = c(list(.local_path = private$.local_path), list(...))
      private$.data = try(rlang::eval_tidy(expr = private$.load, data = local_data))
      if ('try-error' %in% class(private$.data)) {
        message = glue::glue("failed to load remote file ",
          "from cache at {private$.local_path} using command '{cmd}'.", 
          cmd = rlang::quo_text(private$.load))
        private$.logger(message)
        rlang::abort(message = message, load_cmd = private$.load, local_data = local_data)
      }
      return(private$.local_path)
    },
    .save_local = function() {
      from = private$.source_path
      to = private$.local_binary_path
      to %>% fs::path_dir() %>% fs::dir_create()
      private$.logger("saving processed file from '{from}' to '{to}'.", from = from, to = to)
      if (nrow(private$.data) == 0) {
        msg = glue::glue("no data to save to '{to}'.", to = to)
        private$.logger(msg)
        rlang::abort(msg)
      }
      if (fs::path_ext(to) == 'rds') {
        saveRDS(private$.data, file = to)
      } else if (fs::path_ext(to) == 'qs') {
        qs::qsave(private$.data, file = to)
      }
      private$.logger("processed file from '{from}' saved to '{to}'.", from = from, to = to)
      private$.data = tibble::tibble()
      return(to)
    },
    .update_path = function(uri, rpath, data_dir, build_dir, binary_format) {
      private$.uri = uri
      private$.rpath = rpath
      private$.source_path = fs::path(uri, rpath) 
      private$.file_name = fs::path_file(private$.source_path)
      private$.file_name_stub = fs::path_file(private$.source_path) %>% fs::path_ext_remove()
      private$.file_name_ext = fs::path_ext(private$.source_path)
      local_rpath = private$.path_standardizer(rpath)
      private$.local_path = fs::path(data_dir, local_rpath)
      private$.local_path %>% fs::path_dir() %>% fs::dir_create(recurse = TRUE)
      private$.local_dir = fs::path(data_dir, local_rpath)
      local_binary_file = fs::path_file(local_rpath) %>% fs::path_ext_set(binary_format)
      local_binary_dir = fs::path_dir(local_rpath)
      private$.local_binary_path = fs::path(build_dir, local_binary_dir, local_binary_file)
      private$.data_dir = data_dir
      private$.build_dir = build_dir
    },
    .update_artifact_dir = function(path) {
      private$.artifact_dir = path
      private$.logger("updated artifact directory, artifacts may need to be re-created.")
      return(private$.artifact_dir)
    },
    .update_build_dir = function(path) {
      private$.build_dir = path
      private$.logger("updated build directory, builds may need to be re-created.")
      return(private$.build_dir)
    },
    .update_cached_file = function(...) {
      if (fs::file_exists(private$.local_path)) {
        private$.logger("Reusing current file at: {local_path}", local_path = private$.local_path)
      } else {
        private$.logger("Loading file to: {local_path}", local_path = private$.local_path)
        local_data = c(list( 
          .source_path = private$.source_path,
          .local_path = private$.local_path), list(...))
        fetched = try(rlang::eval_tidy(expr = private$.retrieve, data = local_data))
        if ('try-error' %in% class(fetched)) {
          message = glue::glue("failed to retrieve remote file for cache ",
            "from {private$.source_path} using command '{cmd}'.", 
            cmd = rlang::quo_text(private$.retrieve))
          private$.logger(message)
          rlang::abort(message = message, retrieve_cmd = private$.retrieve, local_data = local_data)
        }
      }
      return(private$.local_path)
    }
  ),
  active = list(
    colnames = function() private$.colnames,
    table = function() {
      private$.load_local()
      colnames(private$.data) = self$colnames
      o = private$.data
      private$.data = tibble::tibble()
      return(o)
    },
    source_file = function() private$.file_name,
    source_path = function() private$.source_path,
    definitions = function() private$.definitions,
    print_definitions = function() purrr::map_chr(private$.definitions, ~ .x$as_text()),
    definitions_table = function() purrr::map(private$.definitions, ~ .x$as_list()) %>%
      purrr::lift_dl(dplyr::bind_rows)(),
    artifact_dir = function() private$.artifact_dir,
    build_dir = function() private$.build_dir,
    data_dir = function() private$.data_dir
  )
)

