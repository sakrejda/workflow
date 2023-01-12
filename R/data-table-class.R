

simple_standardizer = function(s) s |> tolower() |> 
  stringr::str_replace_all("[ _]", "-")
  
fix = function(record_id, column, current, new) {
  o = list(
    record_id = record_id, 
    column = column, 
    current = current, 
    new = new)
  class(o) = 'single-fix'
  return(o)
}

pattern_fix = function(column, pattern, replacement, f = stringr::str_replace) {
  o = list(
    column = column,
    pattern = pattern,
    replacement = replacement)
  class(o) = 'pattern-fix'
  return(o)
}

merge_fix = function(data) {
  o = list(
    data = data,
    original_columns = data |>
      dplyr::select(tidyselect::ends_with('__current')) |>
      colnames()
  )
  colnames(o$data)[stringr::str_detect(colnames(o$data), '__current$')] = 
    o$original_columns
  class(o) = 'merge-fix'
  return(o)
}

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
      retrieve = rdrop2::drop_download(
        path = .source_path, local_path = .local_path),
      load = haven::read_sas(data_file = .local_path),
      .data_dir = workflow::data_dir(),
      .build_dir = workflow::build_dir(),
      .artifact_dir = workflow::artifact_dir(),
      .binary_format = "qs",
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
    correct = function(...) {
      args = list(...)
      cl = purrr::map_chr(args, class)
      for (i in seq_along(args)) {
        if (cl[i] == 'list-of-corrections') {
          purrr::lift_dl(self$correct)(args[[i]])
        } else if (cl[i] == 'pattern-fix') {
          private$.load_local()
          column = args[[i]]$column
          record_id_idx = stringr::str_detect(private$.data[[column]], args[[i]]$pattern) |> which()
          fixes = list(
            record_id = private$.data$record_id[record_id_idx],
            column = rep(column, length(record_id_idx)),
            current = private$.data[[column]],
            new = rep(args[[i]]$replacement, length(record_id_idx)))
          fixes = purrr::pmap(fixes, list) |> purrr::map(`class<-`, 'single-fix')
          purrr::lift_dl(self$correct)(fixes)
        } else if (cl[i] == 'single-fix') {
          private$.load_local()
          if (!(args[[i]]$column %in% private$.colnames)) {
            private$.logger("Column '{name}' is not contained",
                            " in the current data.", name = ars[[i]]$column)
          }
          if (!(args[[i]]$record_id %in% private$.data$record_id)) {
            private$.logger("Record id '{record_id}' is not contained",
                            " in the current data.", record_id = args[[i]]$record_id)
          }
          n = length(private$.corrections)
          private$.corrections[[n+1]] = args[[i]]
          private$.logger("For file '{file_name}', ",
                          "inserted correction for column '{column}' ",
                          "in record '{record_id}' from '{from}' to {to}.",
                          file_name = private$.file_name, 
                          column = args[[i]]$column,
                          record_id = args[[i]]$record_id,
                          from = args[[i]]$current, 
                          to = args[[i]]$new)
        } else if (cl[i] == 'merge-fix') {
          n = length(private$.corrections)
          private$.corrections[[n+1]] = args[[i]]
          private$.logger("For file '{file_name}', inserted a 'merge-fix' table.")
        } else {
          msg = glue::glue("Submitted correction must be wrapped.")
          rlang::abort(message = msg, faulty_correction = args[[i]])
        }
      }
      return(self)
    },
    define = function(...) {
      args = list(...)
      names = names(args)
      args_column_metadata = args %>%
        purrr::keep(~ isTRUE("ColumnMetadata" %in% class(.x)))
      args_other = args %>% 
        purrr::keep(~ !isTRUE("ColumnMetadata" %in% class(.x))) %>%
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
      data = private$.data
      data = dplyr::mutate(data, ...)
      private$.data = data
      private$.colnames = colnames(data)
      private$.save_local()
      return(self)
    },
    process_corrections = function() {
      private$.apply_corrections()
      return(self)
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
        msg = paste("Trying to set value of unknown attribute,",
                    "attributes must be named.")
        rlang::abort(msg)
      }
      rlang::abort("Failed to set attribute, bad arguments.", object = self)
      return(self)
    },
    clear_definitions = function() {
      private$.definitions = list()
      return(self)
    },
    switch_disk_format = function(to = "qs") {
      from = fs::path_ext(private$.local_binary_path)
      if (to == from) {
        return(TRUE)
      }
      private$.load_local()
      if (to == "qs") {
        private$.local_binary_path = fs::path_ext_set(
          private$.local_binary_path, "qs")
        private$.local_binary_file = fs::path_ext_set(
          private$.local_binary_file, "qs")
      } else {
        private$.local_binary_path = fs::path_ext_set(
          private$.local_binary_path, "rds")
        private$.local_binary_file = fs::path_ext_set(
          private$.local_binary_file, "rds")
      }
      private$.save_local()
    }
  ),
  private = list(
    .corrections = list(),
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
    .local_binary_dir = fs::path(),
    .local_binary_file = fs::path(),
    .local_binary_path = fs::path(),
    .local_binary_path_hash = character(),
    .dots = list(),
    .data_dir = character(),
    .artifact_dir = character(),
    .build_dir = character(),
    .logger = function(...) logger::log_info(...),
    .attributes = list(),
    .applied = integer(),
    .apply_corrections = function() {
      private$.load_local()
      for (fix in private$.corrections) {
        if (class(fix) == 'single-fix') {
          record_idx = which(private$.data$record_id == fix$record_id)
          if (!isTRUE(length(record_idx) > 0)) {
            msg = glue::glue("Skipping fix: record '{fix$record_id}' is missing.")
          }
          current_val_check = private$.data[record_idx, fix$column] == fix$current
          if (!isTRUE(current_val_check)) {
            msg = glue::glue("Skipping fix: for record '{fix$record_id}' in ",
              "column '{fix$column}' as it does not contain the value ",
              "'{fix$current}'.")
            rlang::warn(msg)
          } else {
            private$.data[record_idx, fix$column] = fix$new
          }
        } else if (class(fix) == 'merge-fix') {
          pre_fix_colnames = colnames(private$.data)
          pre_fix_nrow = nrow(private$.data)
          current_col = fix$original_columns
          new_col = paste0(current_col, '__new')
          other_col = colnames(fix$data)[!(colnames(fix$data) %in% c(current_col, new_col))]
          if (any(new_col %in% names(private$.data))) {
            msg = glue::glue("Conflict: current data uses reserved suffix '__new'.")
            rlang::abort(msg)
          }
          if (length(other_col) != 0) {
            msg = glue::glue("Conflict: merge fix contains extraneous columns.")
            rlang::abort(msg)
          }
          private$.data = private$.data |>
            dplyr::left_join(y = fix$data, by = current_col)
          match = private$.data |>
            dplyr::select(tidyselect::matches(paste0('^', new_columns, '$'))) |>
            purrr::map(c) |> purrr::map(is.na) |>
            purrr::map(all) |> purrr::map(isFALSE) |>
            purrr::flatten_lgl()
          for (i in seq_along(current_col)) {
            private$.data[match, current_col[i]] = private$.data[match, new_col[i]]
          }
          private$.data = private$.data |> 
            dplyr::select(-tidyselect::matches(paste0('^', new_col, '$')))
          if (colnames(private$.data) != pre_fix_colnames) {
            msg = glue::glue("Conflict: merge fix changed final columns of data.")
            rlang::abort(msg)
          }
          if (nrow(private$.data) != pre_fix_nrow) {
            msg = glue::glue("Conflict: merge fix changed the number of data rows.")
            rlang::abort(msg)
          }
        }
      }
      private$.save_local()
    },
    .apply_definitions = function() {
      original_colnames = private$.colnames
      for (i in seq_along(private$.definitions)) {
        if (i %in% private$.applied) {
          next
        }
        name = private$.definitions[[i]]$name
        standard_name = private$.definitions[[i]]$standard_name
        if (!is.na(standard_name) && (name != standard_name)) {
          private$.logger("For file '{file_name}', ",
            "renaming: {current} --> {standard}",
            file_name = private$.file_name, current = name,
            standard = standard_name)
          if (name %in% private$.colnames) {
            col_mask = (self$.colnames == name)
            private$.colnames = private$.colnames %>%
              purrr::map_if(~ isTRUE(.x == name), ~ standard_name, ~ .x) %>%
              purrr::flatten_chr()
            private$.applied = c(private$.applied, i)
          } else {
            msg = glue::glue("For file '{file_name}' renaming failed. ",
              "The data does not contain",
              " a column named '{name}'.",
              file_name = private$.file_name, name = name)
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
        private$.logger("Name '{name}' is not contained",
          " in the current data.", name = x$name)
      }
      n = length(private$.definitions)
      private$.definitions[[n+1]] = x
      if (is.null(x$standard_name)) {
        private$.logger("For file '{file_name}', ",
          "inserted definition for column '{name}'.",
          file_name = private$.file_name, name = x$name)
      } else {
        private$.logger("For file '{file_name}', ",
          "inserted definition for column '{name}' as '{standard_name}'.",
          file_name = private$.file_name, 
          name = x$name, standard_name = x$standard_name)
      }
    },
    .load_cached = function(...) {
      private$.update_cached_file(!!!private$.dots)
      private$.logger("Loading file from: {local_path}", 
        local_path = private$.local_path)
      local_data = c(list(.local_path = private$.local_path), list(...))
      private$.data = try(
        rlang::eval_tidy(expr = private$.load, data = local_data))
      if ("try-error" %in% class(private$.data)) {
        message = glue::glue("failed to load remote file ",
          "from cache at {private$.local_path} using command '{cmd}'.",
          cmd = rlang::quo_text(private$.load))
        private$.logger(message)
        rlang::abort(message = message,
          load_cmd = private$.load, local_data = local_data)
      }
      return(private$.local_path)
    },
    .load_local = function() {
      current_hash = hash_df(private$.data)
      recorded_hash = private$.local_binary_path_hash
      if (isTRUE(current_hash == recorded_hash)) {
        return(private$.local_binary_path)
      }
      private$.local_binary_path = fs::path(
        private$.local_binary_dir, recorded_hash,
        private$.local_binary_file)
      from = private$.local_binary_path
      if (!fs::file_exists(from)) {
        private$.load_cached(!!!private$.dots)
        recovered_hash = hash_df(private$.data)
        if (recovered_hash != recorded_hash) {
          msg = glue::glue("Data for this object could not be recovered.")
          rlang::abort(msg, recorded_hash = recorded_hash)
        }
        return(private$.local_binary_path)
      }
      private$.logger("loading processed file from '{from}'.", from = from)
      if (fs::path_ext(from) == "rds") {
        private$.data = readRDS(file = from)
      } else if (fs::path_ext(from) == "qs") {
        private$.data = qs::qread(from)
      } else {
        rlang::abort("Unknown extension specified for cache file at save.")
      }
      recovered_hash = hash_df(private$.data)
      if (isTRUE(recovered_hash == recorded_hash)) {
        return(from)
      }
      msg = glue::glue("Cached data for this object is corrupted.")
      rlang::abort(msg, recorded_hash = recorded_hash)
    },
    .recode_values = function() {
      private$.load_local()
      for (i in seq_along(private$.definitions)) {
        if (i %in% private$.applied) {
          col = private$.definitions[[i]]$standard_name
        } else {
          col = private$.definitions[[i]]$name
        }
        new = private$.data[[col]]
        if (is.null(new)) 
          next
        vals = private$.definitions[[i]]$values
        if (is.null(vals))
          next

        if (is.list(vals)) {
          for (j in seq_along(vals)) {
            if (is.null(names(vals)[j])) {
              next
            } else {
              for (k in seq_along(vals[[j]])) {
                new[new == vals[[j]][k]] = names(vals)[j]
              }
            }
          }
        } else {
          for (j in seq_along(vals)) {
            if (is.null(names(vals)[j])) {
              next
            } else {
              new[new == vals[j]] = names(vals)[j]
            }
          }
        }
        private$.data[[col]] = new
      }
      private$.save_local()
    },
    .save_local = function() {
      from = private$.source_path
      current_hash = private$.local_binary_path_hash
      new_hash = hash_df(private$.data)
      private$.local_binary_path_hash = new_hash
      to = fs::path(private$.local_binary_dir, new_hash,
        private$.local_binary_file)
      if (isTRUE(current_hash == new_hash)) {
        return(to)
      }
      private$.local_binary_path = to
      to %>% fs::path_dir() %>% fs::dir_create()
      private$.logger("saving processed file from '{from}' to '{to}'.",
        from = from, to = to)
      if (nrow(private$.data) == 0) {
        msg = glue::glue("no data to save to '{to}'.", to = to)
        private$.logger(msg)
        rlang::abort(msg)
      }
      if (fs::path_ext(to) == "rds") {
        saveRDS(private$.data, file = to)
      } else if (fs::path_ext(to) == "qs") {
        qs::qsave(private$.data, file = to)
      } else {
        rlang::abort("Unknown extension specified for cache file.")
      }
      private$.logger("processed file from '{from}' saved to '{to}'.",
        from = from, to = to)
      private$.data = tibble::tibble()
      return(to)
    },
    .update_path = function(uri, rpath, data_dir, build_dir, binary_format) {
      private$.uri = uri
      private$.rpath = rpath
      private$.source_path = fs::path(uri, rpath)
      private$.file_name = fs::path_file(private$.source_path)
      private$.file_name_stub = fs::path_file(private$.source_path) %>%
        fs::path_ext_remove()
      private$.file_name_ext = fs::path_ext(private$.source_path)
      local_rpath = private$.path_standardizer(rpath)
      private$.local_path = fs::path(data_dir, local_rpath)
      private$.local_path %>% fs::path_dir() %>% fs::dir_create(recurse = TRUE)
      private$.local_dir = fs::path(data_dir, local_rpath)
      private$.local_binary_file = fs::path_file(local_rpath) %>%
        fs::path_ext_set(binary_format)
      local_rpath_dir = fs::path_dir(local_rpath)
      private$.local_binary_dir = fs::path(
        build_dir, local_rpath_dir)
      private$.local_binary_path = fs::path(
        private$.local_binary_dir,
        private$.local_binary_file)
      private$.data_dir = data_dir
      private$.build_dir = build_dir
    },
    .update_artifact_dir = function(path) {
      private$.artifact_dir = path
      private$.logger("updated artifact directory, ",
        "artifacts may need to be re-created.")
      return(private$.artifact_dir)
    },
    .update_build_dir = function(path) {
      private$.build_dir = path
      private$.logger("updated build directory, ",
        "builds may need to be re-created.")
      return(private$.build_dir)
    },
    .update_cached_file = function(...) {
      if (fs::file_exists(private$.local_path)) {
        private$.logger("Reusing current file at: {local_path}",
          local_path = private$.local_path)
      } else {
        private$.logger("Loading file to: {local_path}",
          local_path = private$.local_path)
        local_data = c(list( 
          .source_path = private$.source_path,
          .local_path = private$.local_path), list(...))
        fetched = try(rlang::eval_tidy(
          expr = private$.retrieve, data = local_data))
        if ("try-error" %in% class(fetched)) {
          message = glue::glue("failed to retrieve remote file for cache ",
            "from {private$.source_path} using command '{cmd}'.",
            cmd = rlang::quo_text(private$.retrieve))
          private$.logger(message)
          rlang::abort(message = message, 
            retrieve_cmd = private$.retrieve, local_data = local_data)
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
    print_definitions = function() private$.definitions %>%
      purrr::map_chr(, ~ .x$as_text()),
    definitions_table = function() private$.definitions %>%
      purrr::map(, ~ .x$as_list()) %>%
      purrr::lift_dl(dplyr::bind_rows)(),
    artifact_dir = function() private$.artifact_dir,
    build_dir = function() private$.build_dir,
    data_dir = function() private$.data_dir
  )
)

