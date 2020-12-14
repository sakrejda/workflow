

#' Retrieve system environment variable
#'
#' @param name name of variable to retrieve
#' @return variable value to retrieve
#'
#' @export
get_env = function(name) Sys.getenv(name)

#' Set system environment variable
#'
#' @param name name of variable to set
#' @param value value to set the variable to
#' @return value as set 
#'
#' @export
set_env = function(name, value) {
  if (missing(name)) {
    stop(msg[['env-var-name-missing']])
  }
  if (missing(value)) {
    stop(msg[['env-var-value-missing']])
  }
  rlang::exec(Sys.setenv, !!name := value)
  val = Sys.getenv(name)
  if (val != value) {
    stop(msg[['env-var-set-failed']])
  }
  return(val)
}

#' Default environental variable name for a resource name
#'
#' @param name name of a resource dir
#' @return string of the corresponding environmental variable
#'
#' @export
default_dir_var = function(name) {
  path = paste0(toupper(name), "_DIR")
  return(path)
}

