% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unzip-file.R
\name{unzip_file}
\alias{unzip_file}
\title{Unzip a .zip file}
\usage{
unzip_file(path, target = fs::path_dir(path))
}
\arguments{
\item{path}{path to file to unzip}

\item{target}{where to root unzipped file}
}
\value{
paths of extracted files
}
\description{
Unzip a .zip file
}
