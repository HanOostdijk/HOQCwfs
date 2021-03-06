% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WFS_utils.R
\name{WFS_util_replace_names}
\alias{WFS_util_replace_names}
\title{Replace names in a list}
\usage{
WFS_util_replace_names(mylist, onames, nnames)
}
\arguments{
\item{mylist}{A named list}

\item{onames}{Character vector with names to be replaced (case insensitive)}

\item{nnames}{Character vector with corresponding replacements}
}
\value{
A copy of \code{mylist} with names in \code{onames} replaced by the corresponding ones in \code{nnames}
}
\description{
This function uses two character vectors to replace names in a named list. The first character vector
contains the names (case insensitive) that will be replaced and the second character vector contains
their corresponding replacements. It is possible that all, none or some names will be replaced.
}
\details{
In this package the function is used to translate argument names for the case that these names are different
in the WFS versions.
}
\examples{
\dontrun{
WFS_util_replace_names(mylist,
  c('maxFeatures', 'typeName'),
  c('count',       'typeNames') )
}
}
\seealso{
\code{\link[=WFS_util_unify_names]{WFS_util_unify_names()}} , \code{\link[=WFS_util_v12_names]{WFS_util_v12_names()}}
}
