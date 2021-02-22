
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
generate_document = function(path, ..., type = 'html') {
  param_list = list(...)
  rel_path_base = fs::path_dir(path)
  file_name = fs::path_file(path)
  file_path = workflow::project_dir(path)
  output_dir = workflow::artifact_dir(rel_path_base)
  build_dir = workflow::build_dir(rel_path_base)
  rmarkdown::render(
    input = file_path, 
    output_dir = output_dir,
    intermediates_dir = build_dir, 
    knit_root_dir = build_dir,
    params = param_list
  )
  out_path = workflow::artifact_dir(rel_path_base, fs::path_ext_set(file_name, type))
  return(out_path)
}

