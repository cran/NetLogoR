% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agentMatrix-Class-methods.R
\docType{methods}
\name{initialize,agentMatrix-method}
\alias{initialize,agentMatrix-method}
\title{Initialize for \code{agentMatrix} Class}
\usage{
\S4method{initialize}{agentMatrix}(.Object = "agentMatrix", coords, ..., levelsAM)
}
\arguments{
\item{.Object}{ An object:  see the \dQuote{Initialize Methods} section.}

\item{coords}{2 column matrix of coordinates}

\item{...}{arguments to specify properties of the new object, to
      be passed to \code{initialize()}.}

\item{levelsAM}{A list with named character vectors. Each name should
match with elements in \code{...}, and each character vector
should be the length of unique elements in the \code{...} element.}
}
\value{
An \code{agentMatrix} object.
}
\description{
To create a new \code{agentMatrix} object.
}
