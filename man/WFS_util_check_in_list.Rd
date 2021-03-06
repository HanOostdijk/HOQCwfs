% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WFS_utils.R
\name{WFS_util_check_in_list}
\alias{WFS_util_check_in_list}
\title{Check a character value in a named list with character contents}
\usage{
WFS_util_check_in_list(
  mylist,
  name,
  value,
  keep_first = T,
  name_ignore_case = T,
  value_ignore_case = T
)
}
\arguments{
\item{mylist}{A named list with character contents}

\item{name}{Character string with the name of the entry that is searched}

\item{value}{A character scalar to check for in the \code{name} entry}

\item{name_ignore_case}{Logical scalar indicating if case is ignored when searching for the name}

\item{value_ignore_case}{Logical scalar indicating if case is ignored when checking the value}

\item{keep.first}{Logical scalar indicating if the first (when \code{TRUE}) or last entry (when \code{FALSE}) with duplicate names will be used}
}
\value{
Logical value indicating if the \code{name} entry contains the value \code{value}
}
\description{
With the default parameters the function is checking without caring for lower and upper caps
if a list has an entry with a certain name and value. When a name occurs more than once
the first (if \code{keep.first==TRUE}) or last (otherwise) occurrence is examined.
By specifying the \code{ignore_case} arguments to \code{FALSE} the user
can indicate that the difference in lower and upper caps is important when comparing 'names'
(in case of \code{name_ignore_case} ) and/or  'values' (in case of \code{value_ignore_case} ).
}
\examples{
WFS_util_check_in_list(
    mylist = list(a='x',b='y',b='Y'),
    name = 'B',
    value = 'Y',
    value_ignore_case = F
  )
# [1] FALSE  # first entry B found with value y (unequal to Y)
}
