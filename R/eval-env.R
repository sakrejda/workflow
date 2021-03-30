

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
