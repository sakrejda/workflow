#' Check if package can be loaded
#'
#' Loading happens in a separate R process (that should find the same
#' .Renviron, etc... files as the current one so this should be a reproducible
#' check.  Returns FALSE unless the package can be loaded
#'
#' Does not affect state of namespace in the current session!
#'
#' @param pkg name of the package as a string
#' @return TRUE iff package can be loaded
#'
#' @export
try_load_elsewhere = function(pkg) {
  cl = parallel::makePSOCKcluster(names = 'localhost')
  outcome = try(parallel::clusterCall(cl = cl, fun = function(p) {
    o = try(require(package = p, character.only = TRUE))
    return(o)
  }, p = pkg))[[1]]
  if (isTRUE(outcome)) {
    return(TRUE)
  }
  parallel::stopCluster(cl = cl)
  return(FALSE)
}

#' Check if packages can be loaded report problems
#'
#' See `workflow::try_load_elsewhere` for details on check.
#'
#' @param pkgs vector of package names
#' @return vector of package names for packages with load problems
#'
#' @export
detect_load_problems = function(pkgs) {
  problems = character()
  default_packages = getOption("defaultPackages")
  for (pkg in pkgs) {
    if (pkg %in% default_packages)
      next
    loaded = try_load_elsewhere(pkg) 
    if (!isTRUE(loaded))
      problems = c(problems, pkg)
  }
  return(problems) 
}

#' Default place to find list of packages
#'
#' one package name per line
#'
#' @param path where to find list
#' @return path to package list
#'
#' @export
default_pkglist = function(path = "~/.Rpackages") return(path)

#' Retrieve the list of packages that should be installed
#'
#' @param path path to file with default list
#' @return vector of package names to install
#'
#' @export
get_pkglist = function(path = default_pkglist()) {
  if (!file.exists(path)) {
    msg = paste0("The file '", path, "' does not exist.")
  }
  pkgs = readLines(path)
  pkgs = pkgs[pkgs != ""]
  return(pkgs)
}

#' Retrieve list of R's default packages
#'
#' Just avoiding brain freeze
#'
#' @return R's list of default packages
#' @export
get_default_packages = function() getOption("defaultPackages")

#' Install a list of packages
#'
#' @param packages vector of package names to install
#' @return list with namems of installs attempted, successes, and failures
#'
#' @export
install_packages = function(packages = get_pkglist()) {
  lib = assure_user_lib()[1]
  repos = assure_repos()
  try(utils::install.packages(pkgs = packages, lib = lib, repos = repos, dependencies = TRUE))
  problems = detect_load_problems(packages)
  o = list(
    attempted = packages,
    success = setdiff(packages, problems),
    problems = problems)
  return(o)
}

#' @export
old_packages = function(lib_path = assure_user_lib(), repos = assure_repos()) {
  old = old.packages(lib.loc = lib_path, repos = repos)
  return(old)
}

#' @export
update_packages = function(
  packages = old_packages(), 
  lib_path = assure_user_lib(),
  repos = assure_repos()
) {
  old = old_packages(lib_path, repos)
  install_packages(old)
  return(old)
}
   
#' Make sure the path to the user's location for R packages exists
#'
#' @param lib_path where to put packages
#' @return detected library paths
#'
#' @export
assure_user_lib = function(lib_path = Sys.getenv("R_LIBS_USER")) {
  if (!dir.exists(lib_path))
    dir.create(lib_path, recursive=TRUE)
  lp = .libPaths(lib_path)
  return(lp)
}
 
#' Make sure the repos option is set and matches the environmental variable
#'
#' @return URL for repos to use
#'
#' @export
assure_repos = function() {
  current_repos = getOption("repos")
  current_repos['CRAN'] = Sys.getenv("R_CRAN")
  options(repos = current_repos)
  return(getOption("repos"))
}

#' Maks sure the list of packages is installed
#'
#' @param path path to list of packages (one per line)
#' @return
#'
#' @export
assure_package_installation = function(path = default_pkglist()) {
  options(stringsAsFactors=FALSE)
  
  installed_packages = rownames(utils::installed.packages())
  update_packages(installed_packages)
  non_default_packages = setdiff(installed_packages, get_default_packages())
  
  problems = detect_load_problems(installed_packages)
  repair_install = install_packages(problems)
  
  installed_packages = rownames(utils::installed.packages())
  remaining_packages = setdiff(x=get_pkglist(path), y=installed_packages)
  if (length(remaining_packages) != 0) {
    remaining_install = install_packages(remaining_packages)
  }
  remaining_packages = setdiff(x=get_pkglist(path), y=installed_packages)
  return(remaining_packages)
}




