% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filesystem-locations.R
\name{resource}
\alias{resource}
\title{Retrieve the named resource}
\usage{
resource(name, ..., .root = NULL)
}
\arguments{
\item{name}{name of the resource}

\item{...}{additional relative path elements to resource}

\item{.root, }{optionally the new path root}
}
\value{
path to resource, if available
}
\description{
Here a resource is a named location on a filesystem, can be
passed to fs::path, etc..., similar to here::here but doesn't do
a complex search and allows out-of-tree references.
}
