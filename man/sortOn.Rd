% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agentset-functions.R
\name{sortOn}
\alias{sortOn}
\alias{sortOn,matrix,worldMatrix,missing-method}
\alias{sortOn,matrix,worldArray,character-method}
\alias{sortOn,agentMatrix,missing,character-method}
\title{Sort \code{agents}}
\usage{
sortOn(agents, world, var)

\S4method{sortOn}{matrix,worldMatrix,missing}(agents, world)

\S4method{sortOn}{matrix,worldArray,character}(agents, world, var)

\S4method{sortOn}{agentMatrix,missing,character}(agents, var)
}
\arguments{
\item{agents}{Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second
column \code{pycor} representing the \code{patches} coordinates, or

\if{html}{\out{<div class="sourceCode">}}\preformatted{          `AgentMatrix` object representing the moving `agents`.
}\if{html}{\out{</div>}}}

\item{world}{\code{WorldMatrix} or \code{worldArray} object.}

\item{var}{Character. The name of the selected \code{agents} variable.
If \code{agents} are \code{patches} and the \code{world} is a
\code{worldMatrix} object, \code{var} must not be provided. If
\code{agents} are \code{patches} and the \code{world} is a \code{worldArray}
object, \code{var} is the name of the layer to use to define the \code{patches}
values. If \code{agents} are \code{turtles}, \code{var} is one of
the \code{turtles}' variable and can be equal to \code{xcor},
\code{ycor}, any of the variables created when \code{turtles} were created,
as well as any variable created using \code{turtlesOwn()}.}
}
\value{
Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second column
\code{pycor} representing the coordinates of the \code{patches} sorted according to
their values, if \code{agents}
are \code{patches}, or

\if{html}{\out{<div class="sourceCode">}}\preformatted{    `AgentMatrix` representing the `turtles` sorted according
    to their `var` values, if `agents` are
    `turtles`.
}\if{html}{\out{</div>}}
}
\description{
Return the \code{agents} sorted according to their value.
}
\details{
\code{world} must not be provided if \code{agents} are \code{turtles}.

\if{html}{\out{<div class="sourceCode">}}\preformatted{     The sorting of the `agents` is done in a increasing order.
}\if{html}{\out{</div>}}
}
\examples{
# Patches
w1 <- createWorld(
  minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
  data = sample(1:5, size = 25, replace = TRUE)
)
plot(w1)
p1 <- sortOn(agents = patches(w1), world = w1)

# Turtles
t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
sortHeadingT1 <- sortOn(agents = t1, var = "heading")

# or
library(quickPlot)

Plot(w1)
Plot(t1, addTo = "w1")

}
\references{
Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
Center for Connected Learning and Computer-Based Modeling,
Northwestern University. Evanston, IL.
}
\seealso{
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#sort-on}
}
\author{
Sarah Bauduin
}
