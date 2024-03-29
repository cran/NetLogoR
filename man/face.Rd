% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turtle-functions.R
\name{face}
\alias{face}
\alias{face,agentMatrix,matrix-method}
\title{Face something}
\usage{
face(turtles, agents2, world, torus = FALSE)

\S4method{face}{agentMatrix,matrix}(turtles, agents2, world, torus = FALSE)
}
\arguments{
\item{turtles}{\code{AgentMatrix} object representing the moving \code{agents}.}

\item{agents2}{Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second
column \code{pycor} representing the \code{patches} coordinates, or

\if{html}{\out{<div class="sourceCode">}}\preformatted{           `AgentMatrix` object representing the moving `agents`, or

           Matrix (`ncol` = 2) with the first column `x` and the second column
           `y` representing locations coordinates.
}\if{html}{\out{</div>}}}

\item{world}{\code{WorldMatrix} or \code{worldArray} object.}

\item{torus}{Logical to determine if the \code{world} is wrapped. Default is
\code{torus = FALSE}.}
}
\value{
\code{AgentMatrix} representing the \code{turtles} with updated \code{headings}.
}
\description{
Set the \code{turtles}' \code{heading} towards \code{agents2}.
}
\details{
The number of agents/locations in \code{agents2} must be equal to 1 or
to the length of \code{turtles}.

\if{html}{\out{<div class="sourceCode">}}\preformatted{     If `torus = FALSE`, `world` does not need to be provided.

     If `torus = TRUE` and the distance from one `turtles` to
     its corresponding agent/location `agents2` is smaller around the
     sides of the `world` than across it, then the direction to the agent/location
     `agents2` going around the sides of the `world` is given to the `turtle`.

     If a turtle is facing its own location, its heading does not change.
}\if{html}{\out{</div>}}
}
\examples{
w1 <- createWorld(
  minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
  data = runif(25)
)
t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
plot(w1)
points(t1, col = of(agents = t1, var = "color"), pch = 16)

t1 <- face(turtles = t1, agents2 = cbind(x = 0, y = 0))
t1 <- fd(turtles = t1, dist = 0.5)
points(t1, col = of(agents = t1, var = "color"), pch = 16)

}
\references{
Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
Center for Connected Learning and Computer-Based Modeling,
Northwestern University. Evanston, IL.
}
\seealso{
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#face}

\if{html}{\out{<div class="sourceCode">}}\preformatted{     <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#facexy>
}\if{html}{\out{</div>}}
}
\author{
Sarah Bauduin
}
