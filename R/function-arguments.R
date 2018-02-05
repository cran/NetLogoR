################################################################################
#' Function arguments
#'
#' @keywords integral
#'
#' @name fargs
#'
#'
#' @param n Integer.
#'
#' @param world WorldMatrix or worldArray object.
#'
#' @param torus Logical to determine if the \code{world} is wrapped. Default is
#'              \code{torus = FALSE}.
#'
#' @param minPxcor Integer. Minimum pxcor for the patches (world's left border).
#'
#' @param maxPxcor Integer. Maximum pxcor for the patches (world's right border).
#'
#' @param minPycor Integer. Minimum pycor for the patches (world's bottom border).
#'
#' @param maxPycor Integer. Maximum pycor for the patches (world's top border).
#'
#' @param pxcor Integer. Vector of patches pxcor coordinates. Must be of length 1
#'              or of the same length as \code{pycor}.
#'
#' @param pycor Integer. Vector of patches pycor coordinates. Must be of length 1
#'              or of the same length as \code{pxcor}.
#'
#' @param cellNum Integer. Vector of cells number.
#'
#' @param pVar Character. If the \code{world} is a worldArray object, \code{pVar}
#'             is the name of the layer to use to define the patches values.
#'             \code{pVar} must not be provided if the \code{world} is a worldMatrix object.
#'
#' @param turtles AgentMatrix object representing the moving agents.
#'
#' @param patches Matrix (ncol = 2) with the first column "pxcor" and the second
#'                column "pycor" representing the patches coordinates.
#'
#' @param agents Matrix (ncol = 2) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates, or
#'
#'               AgentMatrix object representing the moving agents.
#'
#' @param agents2 Matrix (ncol = 2) with the first column "pxcor" and the second
#'                column "pycor" representing the patches coordinates, or
#'
#'                AgentMatrix object representing the moving agents, or
#'
#'                Matrix (ncol = 2) with the first column "x and the second column
#'                "y" representing locations coordinates.
#'
#' @param nNeighbors Integer: 4 or 8. Represents the number of neighbor patches
#'                   considered.
#'
#' @param dx Numeric. Vector of distances to the east (right) from the \code{agents}.
#'           If \code{dx} is negative, the distance to the west (left) is computed.
#'           \code{dx} must be of length 1 or of the same length as number of patches
#'           or turtles in \code{agents}.
#'
#' @param dy Numeric. Vector of distances to the north (up) from the \code{agents}.
#'           If \code{dy} is negative, the distance to the south is computed (down).
#'           \code{dy} must be of length 1 or of the same length as number of patches
#'           or turtles in \code{agents}.
#'
#' @param color Character. Vector of color names. Must be of length \code{n}.
#'              If missing, colors are assigned using the function \code{rainbow(n)}.
#'
#' @param who Integer. Vector of the "who" numbers for the selected \code{turtles}.
#'
#' @param breed Characters. Vector of "breed" names for the selected \code{turtles}.
#'              If missing, there is no distinction based upon "breed".
#'
#' @param var Character. The name of the selected \code{agents} variable.
#'            If \code{agents} are patches and the \code{world} is a
#'            worldMatrix object, \code{var} must not be provided. If
#'            \code{agents} are patches and the \code{world} is a worldArray
#'            object, \code{var} is the name of the layer to use to define the patches
#'            values. If \code{agents} are turtles, \code{var} is one of
#'            the turtles' variable and can be equal to \code{"xcor"},
#'            \code{"ycor"}, any of the variables created when turtles were created,
#'            as well as any variable created using \code{turtlesOwn()}.
#'
#' @param val Numeric or character. Vector of any length.
#'
#'
NULL
