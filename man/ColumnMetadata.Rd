% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/column-metadata-class.R
\name{ColumnMetadata}
\alias{ColumnMetadata}
\title{A class representing metadata about a single column}
\description{
Describes column contents
}
\details{
It's metadata
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ColumnMetadata-new}{\code{ColumnMetadata$new()}}
\item \href{#method-ColumnMetadata-merge}{\code{ColumnMetadata$merge()}}
\item \href{#method-ColumnMetadata-as_text}{\code{ColumnMetadata$as_text()}}
\item \href{#method-ColumnMetadata-as_list}{\code{ColumnMetadata$as_list()}}
\item \href{#method-ColumnMetadata-clone}{\code{ColumnMetadata$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ColumnMetadata-new"></a>}}
\if{latex}{\out{\hypertarget{method-ColumnMetadata-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ColumnMetadata$new(
  name,
  definition = NA,
  standard_name = standardize_name(name),
  format = NA,
  units = NA,
  values = NA,
  original_name = NA
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ColumnMetadata-merge"></a>}}
\if{latex}{\out{\hypertarget{method-ColumnMetadata-merge}{}}}
\subsection{Method \code{merge()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ColumnMetadata$merge(other, priority = "self")}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ColumnMetadata-as_text"></a>}}
\if{latex}{\out{\hypertarget{method-ColumnMetadata-as_text}{}}}
\subsection{Method \code{as_text()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ColumnMetadata$as_text()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ColumnMetadata-as_list"></a>}}
\if{latex}{\out{\hypertarget{method-ColumnMetadata-as_list}{}}}
\subsection{Method \code{as_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ColumnMetadata$as_list()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ColumnMetadata-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ColumnMetadata-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ColumnMetadata$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
