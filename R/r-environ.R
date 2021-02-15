
#' Take a dotlist of expressions and evaluate them
#'
#' Evaluation environment can be set, it's a new child of the calling
#' environment by default.
#'
#' @param ... dotlist of expressions
#' @param .env evaluation environment
#' @return list of values created in the enviornment
#'
#' @export
eval_env = function(
  ..., 
  .env = rlang::new_environment(parent = rlang::caller_env())
) {
  exprs = rlang::enexprs(...)
  expr_names = names(exprs)
  for (i in seq_along(exprs)) {
    name = expr_names[i]
    value = try(rlang::eval_tidy(expr = exprs[[i]], data = rlang::new_data_mask(.env), env = .env))
    if ('try-error' %in% class(value)) {
      rlang::abort(message = paste0(
        "Building .Renviron failed at argument #", i, ": '", name, "', ",
          "check 'value' using 'rlang::last_error'\n"),
        class = 'failed-expression', value = value, name = name, environment = .env)
    }
    if (name != '') {
      rlang::env_bind(.env = .env, !!name := value)
    }
  }
  values = rlang::env_get_list(env = .env, nms = expr_names[expr_names != ''])
  return(values)
}

#' Default environment values for the workflow
#'
#' @param project_name arbitrary project name
#' @param project_version three-segment current version
#' @param project_dir project source files directory
#' @param build_dir project scratch space
#' @param artifact_dir specified project outputs will go here
#' @param data_dir root directory for reaching project data sets, primarily
#'                 used as read-only
#' @param config_dir configuration files and tokens stored here
#' @param r_libs where library files are found
#' @param r_libs_user where library files are found, same as r_libs
#' @param r_cran repos to use
#' @param r_pkg_list name of file storing R packages list for project
#' @return list of these defaults, modified by args if specified.
#'
#' @export
get_default_environ = function(project_name) {
  project_name = project_name
  project_version = "00.00.001"
  project_dir = getwd()
  build_dir = rappdirs::user_cache_dir(
    appname = project_name, version = project_version)
  artifact_dir = rappdirs::user_data_dir(
    appname = project_name, version = project_version)
  data_dir = rappdirs::user_data_dir(
    appname = project_name, version = project_version)
  config_dir = rappdirs::user_config_dir(
    appname = project_name, version = project_version)
  r_libs = file.path(build_dir, ".R/library")
  r_libs_user = file.path(build_dir, ".R/library")
  r_cran = "https://cloud.r-project.org"
  r_pkg_list = ".Rpackages"
  defaults = list(
    project_name = project_name,
    project_version = project_version,
    build_dir = build_dir,
    artifact_dir = artifact_dir,
    data_dir = data_dir,
    config_dir = config_dir,
    r_libs = r_libs,
    r_libs_user = r_libs_user,
    r_cran = r_cran,
    r_pkg_list = r_pkg_list)
  return(defaults)
}


#' Create a list of values to send to .Renviron file
#'
#' Any default-required values missing from inputs are filled
#' in from defaults (see `?get_default_environ`)
#'
#' @param ... values to modify from defaults 
#' @param .env environment to evaluate the arguments in, 
#'        child of caller by default
#' @return list of environment values
#'
#' @export
build_environ_list = function(
  ...,
  .env = rlang::new_environment(parent = rlang::caller_env())
) {
  values = eval_env(..., .env = .env)
  if ('project_name' %in% names(values)) {
    defaults = get_default_environ(project_name = values[['project_name']])
  } else {
    stop("'project_name' is a required argument.")
  }
  for (i in seq_along(defaults)) {
    default_name = names(defaults)[i]
    default_value = defaults[[i]]
    if (!(default_name %in% names(values))) {
      values[[default_name]] = default_value
    }
  }
  return(values)
}

#' Format name/value pair for .Renviron file
#'
#' @param name .Renviron variable name
#' @param var .Renviron variable value
#' @return formatted string
#'
#' @export
e_string = function(name, var) paste0(name, '="', var, '"')

#' Write list of values to .Renviron file in correct format
#'
#' Any default-required values missing from inputs are filled
#' in from defaults (see `?get_default_environ`)
#'
#' Warning: the default .Renviron path assumes the function is running
#' with the working directory set to the project directory.
#'
#' @param ... values to modify from defaults 
#' @param .env environment to evaluate the arguments in, 
#'        child of caller by default
#' @param .Renviron path to .Renviron file for the project
#' @return list of environment values, these are also written to the 
#'         specified .Renviron file *and* read into the current session
#'         using `readRenviron`
#'
#' @export
set_environ = function(
  ..., 
  .env = rlang::new_environment(parent = rlang::caller_env()),
  .Renviron = ".Renviron"
) {
  args = rlang::enquos(...)
  e_list = build_environ_list(!!!args, .env = .env)
  s = character()
  for (i in seq_along(e_list)) {
    name = names(e_list)[i]
    var = toupper(name)
    value = e_list[[i]]
    s = c(s, e_string(var, value))
  }
  s = paste(s, collapse = "\n")
  cat(s, file = .Renviron, append = FALSE)
  readRenviron(.Renviron)
  return(e_list)
}



