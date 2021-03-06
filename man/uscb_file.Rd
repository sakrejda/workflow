% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource-uscb.R
\name{uscb_file}
\alias{uscb_file}
\title{File name munging from characteristics}
\usage{
uscb_file(
  year = 2019,
  area = "us",
  type = "block",
  format = "gdb",
  location = "tiger"
)
}
\arguments{
\item{year}{in four digits}

\item{area}{area the file covers}

\item{type}{spatial data type (block by default)}

\item{format}{currently only 'gdb' (default)}

\item{location}{currently only 'tiger' (default)}
}
\value{
file name for the resource
}
\description{
File name munging from characteristics
}
