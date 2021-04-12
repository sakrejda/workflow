
#' Render an rmarkdown doc (.Rmd)
#'
#' FIXME: currently type *must* be html... :/
#'
#' Convention is that project contains the doc itself, garbage ends up in 
#' build_dir and output ends up in artifact dir.
#'
#' @param path (relative to project root) to the .Rmd file
#' @param ... parameters to pass to the markdown document
#' @param type file extension on the output (.html)
#' @return path to output document
#'
#' @export
generate_document = function(template, rel_output_path, ..., type = 'html') {
  param_list = list(...)
  file_name = fs::path_file(template)
  out_path = workflow::artifact_file(rel_output_path, fs::path_ext_set(file_name, type))
  template_path = workflow::project_file(template)
  if (!fs::file_exists(template_path)) {
    rlang::abort(msg = glue::glue("template not found: '{file}'", file = template_path),
                 missing_file = template_path)
  }
  output_dir = workflow::artifact_dir(rel_output_path)
  build_dir = workflow::build_dir(rel_output_path)
  rmarkdown::render(
    input = template_path,
    output_dir = output_dir,
    intermediates_dir = build_dir, 
    knit_root_dir = build_dir,
    params = param_list
  )
  return(out_path)
}

