
#' List of messages returned by the package
#'
#' In the functions they are referred to as objects.
#'
#' @export
msg = list(
  `resource-missing-path` = "A missing a path can not be used as a resource directory.",
  `resource-missing-name` = "A missing name can not be used as a resource directory name.",
  `resource-dir-is-file` = "A file path can not be used as a resource directory.",
  `resource-dir-is-not-dir` = "Resource directory path is not a file but it is not a directory.",
  `resource-dir-creation-failed` = "Failed to confirm that resource directory exists after creation.",
  `resource-dir-var-empty` = "The variable holding the resource directory location is missing but needed.",
  `resource-not-found` = "Resource not found, using temporary directory instead.",
  `resource-not-found-error` = "Resource not found, stoping.",
  `path-is-null` = "A path can not contain the NULL value.",
  `path-is-empty` = "A path can not be the empty string.",
  `path-is-vector` = "A path can not be a vector.",
  `env-var-set-failed` = "Setting a system environmental variable failed.",
  `env-var-name-missing` = "A missing name can not be used to set an environmental variable.",
  `env-var-value-missing` = "A missing value can not be used to set an environmental variable."
)

