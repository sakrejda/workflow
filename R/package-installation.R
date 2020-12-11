
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

default_pkglist = function() "~/.Rpackages"

get_pkglist = function(path = default_pkglist()) {
  if (!file.exists(path)) {
    msg = paste0("The file '", path, "' does not exist.")
  }
  pkgs = readLines(path)
  pkgs = pkgs[pkgs != ""]
  return(pkgs)
}

get_default_packages = function() getOption("defaultPackages")


install_packages = function(packages = get_pkglist()) {
  libs = assure_user_lib()
  repos = assure_repos()
  try(utils::install.packages(packages))
  problems = detect_load_problems(packages)
  o = list(
    attempted = packages,
    success = setdiff(packages, problems),
    problems = problems)
  return(o)
}
   

assure_user_lib = function(lib_path = Sys.getenv("R_LIBS_USER")) {
  if (!dir.exists(lib_path))
    dir.create(lib_path, recursive=TRUE)
  lp = .libPaths(lib_path)
  return(lp)
}
  
assure_repos = function() {
  current_repos = getOption("repos")
  current_repos['CRAN'] = Sys.getenv("R_CRAN")
  options(repos = current_repos)
  return(getOption("repos"))
}

assure_package_installation = function(path = default_pkglist()) {
  options(stringsAsFactors=FALSE)
  
  installed_packages = rownames(utils::installed.packages())
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




