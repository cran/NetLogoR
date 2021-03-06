% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turtle-functions.R
\name{home}
\alias{home}
\alias{home,worldNLR,agentMatrix,character-method}
\title{Return home}
\usage{
home(world, turtles, home)

\S4method{home}{worldNLR,agentMatrix,character}(world, turtles, home)
}
\arguments{
\item{world}{\code{WorldMatrix} or \code{worldArray} object.}

\item{turtles}{\code{AgentMatrix} object representing the moving \code{agents}.}

\item{home}{Character. Can take one of the following options to define where
               to relocate the \code{turtles}:

               \code{home = "home0"} will place the \code{turtles} at the location
               \code{x = 0, y = 0}.

               \code{home = "center"} will place the \code{turtles} at the center of
               the \code{world}.

               \code{home = "pCorner"} will place the \code{turtles} at the center of
               the \code{patch} located in the left bottom corner of the \code{world}.

               \code{home = "corner"} will place the \code{turtles} at the left bottom
               corner of the \code{world}.}
}
\value{
\code{AgentMatrix} representing the \code{turtles} with updated
        coordinates and updated data for their previous coordinates \code{prevX}
        and \code{prevY}.
}
\description{
Move the \code{turtles} back \code{home}.
}
\examples{
w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
                          data = runif(25))
t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
plot(w1)
points(t1, col = "black", pch = 16)

t1 <- home(world = w1, turtles = t1, home = "pCorner")
points(t1, col = "red", pch = 16)


}
\references{
Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
            Center for Connected Learning and Computer-Based Modeling,
            Northwestern University. Evanston, IL.
}
\seealso{
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#home}
}
\author{
Sarah Bauduin
}
