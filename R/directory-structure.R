
#' Retrieve (weak-reference style), the named resource
#'
#' @param name name of the resource
#' @param path, optionally the path to place the resource at
#' @return path to resource, if available
#' 
#' @export
get_resource = function(name, path = NULL) {
  var = default_dir_var(name)
  val = get_env(var)
  if (val == "") { # $VAR is not defined
    if (is.null(path)) { # no location provided
      stop(msg[['resource-dir-var-empty']])
    }
  } else { # $VAR is defined
    if (is.null(path)) {
      path = val # using found path
    }
  }
  set_env(var, path)
  path = resource_dir(path = path, name = var)
  return(path)
}

#' @export
data_dir = function() get_resource("data")

#' @export
artifact_dir = function() get_resource("artifact")

#' @export
build_dir = function() get_resource("build")
