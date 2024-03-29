% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spades-functions.R
\name{updateList}
\alias{updateList}
\alias{updateList,list,list-method}
\alias{updateList,NULL,list-method}
\alias{updateList,list,NULL-method}
\alias{updateList,NULL,NULL-method}
\title{Update elements of a named list with elements of a second named list}
\usage{
updateList(x, y)

\S4method{updateList}{list,list}(x, y)

\S4method{updateList}{`NULL`,list}(x, y)

\S4method{updateList}{list,`NULL`}(x, y)

\S4method{updateList}{`NULL`,`NULL`}(x, y)
}
\arguments{
\item{x, y}{a named list}
}
\value{
A named list, with elements sorted by name.
The values of matching elements in list \code{y}
replace the values in list \code{x}.
}
\description{
Merge two named list based on their named entries.
Where any element matches in both lists, the value from the second list is
used in the updated list.
Subelements are not examined and are simply replaced. If one list is empty,
then it returns the other one, unchanged.
}
\examples{
L1 <- list(a = "hst", b = NA_character_, c = 43)
L2 <- list(a = "gst", c = 42, d = list(letters))
updateList(L1, L2)

updateList(L1, NULL)
updateList(NULL, L2)
updateList(NULL, NULL) # should return empty list

}
\author{
Alex Chubaty
}
