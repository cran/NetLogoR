if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("from", "id", "to"))
}

################################################################################
#' Diffuse values in a \code{world}
#'
#' Each \code{patch} gives an equal share of a portion of its value to its neighbor \code{patches}.
#'
#' @inheritParams fargs
#'
#' @param share      Numeric. Value between 0 and 1 representing the portion of
#'                   the \code{patches} values to be diffused among the neighbors.
#'
#' @return \code{WorldMatrix} or \code{worldArray} object with \code{patches} values updated.
#'
#' @details What is given is lost for the patches.
#'
#'          If \code{torus = TRUE}, all \code{patches} have \code{nNeighbors} \code{patches} around
#'          them, which
#'          some may be on the other sides of the \code{world}. If \code{torus = FALSE},
#'          \code{patches} located on the edges of the \code{world} have less than
#'          \code{nNeighbors} \code{patches} around them.
#'          However, each neighbor still gets 1/4 or 1/8 of the shared amount
#'          and the diffusing
#'          patch keeps the leftover.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#diffuse}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#diffuse4}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:3, size = 25, replace = TRUE))
#' plot(w1)
#' # Diffuse 50% of each patch value to its 8 neighbors
#' w2 <- diffuse(world = w1, share = 0.5, nNeighbors = 8)
#' plot(w2)
#'
#'
#' @export
#' @importFrom data.table data.table ':='
#' @importFrom data.table setkey
#' @importFrom SpaDES.tools adj
#' @importFrom plyr count
#' @rdname diffuse
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "diffuse",
  function(world, pVar, share, nNeighbors, torus = FALSE) {
    standardGeneric("diffuse")
})

#' @export
#' @rdname diffuse
setMethod(
  "diffuse",
  signature = c(world = "worldMatrix", pVar = "missing", share = "numeric",
                nNeighbors = "numeric"),
  definition = function(world, share, nNeighbors, torus) {

    val <- as.numeric(t(world@.Data))
    cellNum <- 1:length(val)
    toGive <- (val * share) / nNeighbors

    df <- adj(world@.Data, cells = cellNum, directions = nNeighbors, torus = torus)
    nNeigh <- plyr::count(df[, "from"])
    toGiveNeigh <- rep(toGive, nNeigh$freq)
    df <- df[order(df[, "from"]), ]
    dt <- data.table(df, toGiveNeigh)
    setkey(dt, from)
    dt <- dt[, loose := sum(toGiveNeigh), by = from] # how much each patch give
    loose <- unique(dt[, c(1, 4), with = FALSE]) # from and loose
    setkey(dt, to)
    dt <- dt[, win := sum(toGiveNeigh), by = to] # how much each patch receive
    win <- unique(dt[, c(2, 5), with = FALSE]) # to and win

    newVal <- val - loose[, loose] + win[, win]
    world[] <- newVal
    return(world)

})

#' @export
#' @rdname diffuse
setMethod(
  "diffuse",
  signature = c(world = "worldArray", pVar = "character", share = "numeric",
                nNeighbors = "numeric"),
  definition = function(world, pVar, share, nNeighbors, torus) {

    layer <- match(pVar, dimnames(world)[[3]])
    val <- as.numeric(t(world@.Data[, , layer]))
    cellNum <- 1:length(val)
    toGive <- (val * share) / nNeighbors

    df <- adj(world@.Data[, , layer], cells = cellNum, directions = nNeighbors,
              torus = torus)
    nNeigh <- plyr::count(df[, "from"])
    toGiveNeigh <- rep(toGive, nNeigh$freq)
    df <- df[order(df[, "from"]), ]
    dt <- data.table(df, toGiveNeigh)
    setkey(dt, from)
    dt <- dt[, loose := sum(toGiveNeigh), by = from] # how much each patch give
    loose <- unique(dt[, c(1, 4), with = FALSE]) # from and loose
    setkey(dt, to)
    dt <- dt[, win := sum(toGiveNeigh), by = to] # how much each patch receive
    win <- unique(dt[, c(2, 5), with = FALSE]) # to and win

    newVal <- val - loose[, loose] + win[, win]
    world@.Data[, , layer] <- matrix(newVal, ncol = dim(world)[2], byrow = TRUE)

    return(world)
})

################################################################################
#' Distances between agents
#'
#' Report the distances between \code{agents} and \code{agents2}.
#'
#' @inheritParams fargs
#'
#' @param allPairs Logical. Only relevant if the number of agents/locations in
#'                 \code{agents} and in \code{agents2} are the same. If
#'                 \code{allPairs = FALSE},
#'                 the distance between each \code{agents} with the
#'                 corresponding \code{agents2} is returned. If
#'                 \code{allPairs = TRUE}, a full
#'                 distance matrix is returned. Default is \code{allPairs = FALSE}.
#'
#' @return Numeric. Vector of distances between \code{agents} and \code{agents2} if
#'         \code{agents} and/or \code{agents2} contained
#'         one agent/location, or if \code{agents} and \code{agents2} contained the same
#'         number of agents/locations and \code{allPairs = FALSE}, or
#'
#'         Matrix of distances between \code{agents} (rows) and
#'         \code{agents2} (columns)
#'         if \code{agents} and \code{agents2} are of different lengths,
#'         or of same length
#'         and \code{allPairs = TRUE}.
#'
#' @details Distances from/to a patch are measured from/to its center.
#'
#'          If \code{torus = FALSE}, \code{world} does not need to be provided.
#'
#'          If \code{torus = TRUE}, a distance around the sides of the \code{world} is
#'          reported only if smaller than the one across the \code{world}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#distance}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#distancexy}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' NLdist(agents = patch(w1, 0, 0), agents2 = patch(w1, c(1, 9), c(1, 9)))
#' NLdist(agents = patch(w1, 0, 0), agents2 = patch(w1, c(1, 9), c(1, 9)),
#'        world = w1, torus = TRUE)
#' t1 <- createTurtles(n = 2, coords = randomXYcor(w1, n = 2))
#' NLdist(agents = t1, agents2 = patch(w1, c(1,9), c(1,9)), allPairs = TRUE)
#'
#'
#' @export
#' @rdname NLdist
#' @aliases dist
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLdist",
  function(agents, agents2, world, torus = FALSE, allPairs = FALSE) {
    standardGeneric("NLdist")
})

#' @export
#' @rdname NLdist
setMethod(
  "NLdist",
  signature = c(agents = "matrix", agents2 = "matrix"),
  definition = function(agents, agents2, world, torus, allPairs) {

    if (inherits(agents, "agentMatrix")) {
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    if (inherits(agents2, "agentMatrix")) {
      agents2 <- agents2@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    dist <- pointDistance(p1 = agents, p2 = agents2, lonlat = FALSE, allpairs = allPairs)

    if (torus == TRUE) {
      if (missing(world)) {
        stop("A world must be provided as torus = TRUE")
      }

      # Need to create coordinates for "agents2" in a wrapped world
      # For all the 8 possibilities of wrapping (to the left, right, top, bottom and 4 corners)
      to1 <- cbind(pxcor = agents2[, 1] - (world@extent@xmax - world@extent@xmin),
                   pycor = agents2[, 2] + (world@extent@ymax - world@extent@ymin))
      to2 <- cbind(pxcor = agents2[, 1], pycor = agents2[, 2] +
                     (world@extent@ymax - world@extent@ymin))
      to3 <- cbind(pxcor = agents2[, 1] + (world@extent@xmax - world@extent@xmin),
                   pycor = agents2[, 2] + (world@extent@ymax - world@extent@ymin))
      to4 <- cbind(pxcor = agents2[, 1] - (world@extent@xmax - world@extent@xmin),
                   pycor = agents2[, 2])
      to5 <- cbind(pxcor = agents2[, 1] + (world@extent@xmax - world@extent@xmin),
                   pycor = agents2[, 2])
      to6 <- cbind(pxcor = agents2[, 1] - (world@extent@xmax - world@extent@xmin),
                   pycor = agents2[, 2] - (world@extent@ymax - world@extent@ymin))
      to7 <- cbind(pxcor = agents2[, 1], pycor = agents2[, 2] -
                     (world@extent@ymax - world@extent@ymin))
      to8 <- cbind(pxcor = agents2[, 1] + (world@extent@xmax - world@extent@xmin),
                   pycor = agents2[, 2] - (world@extent@ymax - world@extent@ymin))

      dist1 <- pointDistance(p1 = agents, p2 = to1, lonlat = FALSE, allpairs = allPairs)
      dist2 <- pointDistance(p1 = agents, p2 = to2, lonlat = FALSE, allpairs = allPairs)
      dist3 <- pointDistance(p1 = agents, p2 = to3, lonlat = FALSE, allpairs = allPairs)
      dist4 <- pointDistance(p1 = agents, p2 = to4, lonlat = FALSE, allpairs = allPairs)
      dist5 <- pointDistance(p1 = agents, p2 = to5, lonlat = FALSE, allpairs = allPairs)
      dist6 <- pointDistance(p1 = agents, p2 = to6, lonlat = FALSE, allpairs = allPairs)
      dist7 <- pointDistance(p1 = agents, p2 = to7, lonlat = FALSE, allpairs = allPairs)
      dist8 <- pointDistance(p1 = agents, p2 = to8, lonlat = FALSE, allpairs = allPairs)

      dist <- pmin(dist, dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8)
    }

    return(dist)
})

################################################################################
#' Do the patches exist?
#'
#' Report \code{TRUE} if a patch exists inside the \code{world}'s extent, report
#' \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @return Logical.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#member}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pExist(world = w1, pxcor = -1, pycor = 2)
#'
#'
#' @export
#' @rdname pExist
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "pExist",
  function(world, pxcor, pycor) {
    standardGeneric("pExist")
})

#' @export
#' @rdname pExist
setMethod(
  "pExist",
  signature = c("worldNLR", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {

    if (length(pxcor) == 1 & length(pycor) != 1) {
      pxcor <- rep(pxcor, length(pycor))
    }
    if (length(pycor) == 1 & length(pxcor) != 1) {
      pycor <- rep(pycor, length(pxcor))
    }

    pxcorIn <- pxcor >= world@minPxcor & pxcor <= world@maxPxcor
    pycorIn <- pycor >= world@minPycor & pycor <= world@maxPycor
    pExist <- pxcorIn & pycorIn

    return(pExist)
})

################################################################################
#' Neighbors \code{patches}
#'
#' Report the coordinates of the neighbors \code{patches} around the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @return Matrix (\code{ncol} = 3) with the first column \code{pxcor}
#'         and the second column \code{pycor} representing the coordinates of the neighbors
#'         \code{patches} around the \code{agents} and the third column \code{id} representing
#'         the \code{id} of the \code{agents} in the order provided.
#'
#' @details The \code{patch} around which the neighbors are identified, or the \code{patch} where
#'          the \code{turtle} is located on around which the neighbors are identified, is not
#'          returned.
#'
#'          If \code{torus = FALSE}, \code{agents} located on the edges of the
#'          \code{world} have less than \code{nNeighbors} patches around them.
#'          If \code{torus = TRUE}, all \code{agents} located on the edges of the
#'          \code{world} have \code{nNeighbors} patches around them,
#'          which some may be on the other sides of the \code{world}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#neighbors}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' neighbors(world = w1, agents = patch(w1, c(0,9), c(0,7)), nNeighbors = 8)
#' t1 <- createTurtles(n = 3, coords = randomXYcor(w1, n = 3))
#' neighbors(world = w1, agents = t1, nNeighbors = 4)
#'
#'
#' @export
#' @importFrom data.table data.table setkey
#' @importFrom SpaDES.tools adj
#' @rdname neighbors
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "neighbors",
  function(world, agents, nNeighbors, torus = FALSE) {
    standardGeneric("neighbors")
})

#' @export
#' @rdname neighbors
setMethod(
  "neighbors",
  signature = c(world = "worldNLR", agents = "matrix", nNeighbors = "numeric"),
  definition = function(world, agents, nNeighbors, torus) {

    if (inherits(agents, "agentMatrix")) {
      agents <- patch(world = world, x = agents@.Data[, "xcor"],
                      y = agents@.Data[, "ycor"], duplicate = TRUE)
    }

    # To be used with adj()
    if (inherits(world, "worldMatrix")) {
      worldMat <- world@.Data
    } else {
      # worldArray
      worldMat <- world@.Data[, , 1]
    }

    if (nrow(agents) < 100000) {
      # df is faster below 100 agents, DT faster above
      cellNum <- cellFromPxcorPycor(world = world, pxcor = agents[, 1], pycor = agents[, 2])
      neighbors <- adj(worldMat, cells = cellNum, directions = nNeighbors,
                       torus = torus, id = seq_along(cellNum))
      pCoords <- PxcorPycorFromCell(world = world, cellNum = neighbors[, 2])
      neighborsDf <- data.frame(neighbors, pCoords)

      # Output as a matrix
      neighborsDf <- neighborsDf[order(neighborsDf$id), ]
      neighborsID <- cbind(pxcor = neighborsDf$pxcor, pycor = neighborsDf$pycor,
                           id = neighborsDf$id)

    } else {
      cellNum <- cellFromPxcorPycor(world = world, pxcor = agents[, 1], pycor = agents[, 2])
      neighbors <- data.table(adj(worldMat, cells = cellNum, directions = nNeighbors,
                                  torus = torus, id = seq_along(cellNum)))
      cellNum <- data.table(cellNum = cellNum, id = seq_along(cellNum))
      pCoords <- PxcorPycorFromCell(world = world, cellNum = neighbors[, to])
      neighbors[, `:=`(pxcor = pCoords[, 1], pycor = pCoords[, 2])]
      setkey(neighbors, id)
      neighborsID <- cbind(pxcor = neighbors$pxcor,
                           pycor = neighbors$pycor,
                           id = neighbors$id) # %>% as.factor %>% as.numeric)
    }

    return(neighborsID)
})

################################################################################
#' \code{Patches} coordinates
#'
#' Report the coordinates of the \code{patches} at the given \code{[x, y]} locations.
#'
#' @inheritParams fargs
#'
#' @param x          Numeric. Vector of \code{x} coordinates. Must be of same
#'                   length as \code{y}.
#'
#' @param y          Numeric. Vector of \code{y} coordinates. Must be of same
#'                   length as \code{x}.
#'
#' @param duplicate  Logical. If more than one location \code{[x, y]}
#'                   fall into the same \code{patch} and \code{duplicate == TRUE}, the
#'                   \code{patch} coordinates are returned the number of times the locations.
#'                   If \code{duplicate == FALSE}, the \code{patch} coordinates
#'                   are only returned once.
#'                   Default is \code{duplicate == FALSE}.
#'
#' @param out        Logical. If \code{out = FALSE}, no \code{patch} coordinates are returned
#'                   for \code{patches} outside of the \code{world}'s extent, if \code{out = TRUE},
#'                   \code{NA} are returned.
#'                   Default is \code{out = FALSE}.
#'
#' @return Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the \code{patches} coordinates at \code{[x, y]}.
#'
#' @details If a location \code{[x, y]} is outside the \code{world}'s extent and
#'          \code{torus = FALSE} and \code{out = FALSE}, no \code{patch} coordinates are returned;
#'          if \code{torus = FALSE} and \code{out = TRUE}, \code{NA} are returned;
#'          if \code{torus = TRUE}, the \code{patch} coordinates from a wrapped \code{world} are
#'          returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4))
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4),
#'       duplicate = TRUE)
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4),
#'       torus = TRUE)
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4),
#'       torus = TRUE, duplicate = TRUE)
#'
#'
#' @export
#' @rdname patch
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patch",
  function(world, x, y, duplicate = FALSE, torus = FALSE, out = FALSE) {
    standardGeneric("patch")
})

#' @export
#' @rdname patch
setMethod(
  "patch",
  signature = c(world = "worldNLR", x = "numeric", y = "numeric"),
  definition = function(world, x, y, duplicate, torus, out) {

    pxcor <- round(x)
    pycor <- round(y)

    if (torus == TRUE) {
      pCoords <- wrap(cbind(x = pxcor, y = pycor), world@extent)
      pxcor <- pCoords[, 1]
      pycor <- pCoords[, 2]
    }

    pxcor[pxcor < world@minPxcor | pxcor > world@maxPxcor] <- NA
    pycor[pycor < world@minPycor | pycor > world@maxPycor] <- NA
    pxcor[is.na(pycor)] <- NA
    pycor[is.na(pxcor)] <- NA

    if (out == FALSE) {
      pxcor <- pxcor[!is.na(pxcor)]
      pycor <- pycor[!is.na(pycor)]
    }

    pCoords <- matrix(data = c(pxcor, pycor), ncol = 2,
                      nrow = length(pxcor), dimnames = list(NULL, c("pxcor", "pycor")))

    if (duplicate == FALSE) {
      pCoords <- unique(pCoords)
    }
    return(pCoords)
})

################################################################################
#' No \code{patches}
#'
#' Report an empty \code{patch} \code{agentset}.
#'
#' @return Matrix (\code{ncol} = 2, \code{nrow} = 0) with the first column \code{pxcor} and the
#'         second column \code{pycor}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#no-patches}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' p1 <- noPatches()
#' NLcount(p1)
#'
#' @export
#' @rdname noPatches
#'
#' @author Sarah Bauduin
#'
noPatches <- function() {
  return(matrix(0, nrow = 0, ncol = 2, dimnames = list(NULL, c("pxcor", "pycor"))))
}

################################################################################
#' \code{Patches} at
#'
#' Report the coordinates of the \code{patches} at \code{(dx, dy)} distances of the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @return Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates of the \code{patches} at \code{(dx, dy)}
#'         distances of the \code{agents}. The order of the \code{patches} follows the order
#'         of the \code{agents}.
#'
#' @details If the \code{patch} at distance \code{(dx, dy)}
#'          of an \code{agent} is outside of the \code{world}'s extent and \code{torus = FALSE},
#'          \code{NA} are returned
#'          for the \code{patch} coordinates;
#'          if \code{torus = TRUE}, the \code{patch} coordinates from a wrapped \code{world} are
#'          returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-at}
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#at-points}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patchCorner <- patchAt(world = w1, agents = patch(w1, 0, 0), dx = 1, dy = 1)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0))
#' patchCorner <- patchAt(world = w1, agents = t1, dx = 1, dy = 1)
#'
#'
#' @export
#' @rdname patchAt
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchAt",
  function(world, agents, dx, dy, torus = FALSE) {
    standardGeneric("patchAt")
})

#' @export
#' @rdname patchAt
setMethod(
  "patchAt",
  signature = c(world = "worldNLR", agents = "matrix", dx = "numeric", dy = "numeric"),
  definition = function(world, agents, dx, dy, torus) {

    if (inherits(agents, "agentMatrix")) {
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    pxcor <- agents[, 1] + dx
    pycor <- agents[, 2] + dy
    pAt <- patch(world = world, x = pxcor, y = pycor, duplicate = TRUE,
                 torus = torus, out = TRUE)

    return(pAt)
})

################################################################################
#' \code{Patches} at given distances and directions
#'
#' Report the coordinates of the \code{patches} at the given
#' distances and directions from the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @param dist   Numeric. Vector of distances from the \code{agents}. Must be
#'               of length 1 or of the same length as the number of \code{agents}.
#'
#' @param angle  Numeric. Absolute directions from the \code{agents}. \code{angle}
#'               must be of length 1 or of the same length as the number of
#'               \code{agents}. Angles are in degrees with 0 being North.
#'
#' @return Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates of the \code{patches} at the distances
#'         \code{dist} and directions \code{angle} of \code{agents}.
#'         The order of the \code{patches} follows the order of the \code{agents}.
#'
#' @details If \code{torus = FALSE} and the \code{patch} at distance \code{dist} and
#'          direction \code{angle} of an \code{agent} is outside the \code{world}'s extent,
#'          \code{NA} are returned for the \code{patch} coordinates.
#'          If \code{torus = TRUE}, the \code{patch} coordinates from a wrapped \code{world}
#'          are returned.
#'
#'          If \code{agents} are \code{turtles}, their \code{headings} are not taken into account;
#'          the given directions \code{angle} are used. To find a \code{patch} at certain
#'          distance from a \code{turtle} using the \code{turtle}'s \code{heading}, look at \code{pacthAhead()},
#'          \code{patchLeft()} or \code{patchRight()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-at-heading-and-distance}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- patchDistDir(world = w1, agents = patch(w1, 0, 0), dist = 1, angle = 45)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 315)
#' p2 <- patchDistDir(world = w1, agents = t1, dist = 1, angle = 45)
#'
#' @export
#' @importFrom CircStats rad
#' @rdname patchDistDir
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchDistDir",
  function(world, agents, dist, angle, torus = FALSE) {
    standardGeneric("patchDistDir")
})

#' @export
#' @rdname patchDistDir
setMethod(
  "patchDistDir",
  signature = c(world = "worldNLR", agents = "matrix", dist = "numeric", angle = "numeric"),
  definition = function(world, agents, dist, angle, torus) {
    if (inherits(agents, "agentMatrix")) {
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    radAngle <- rad(angle)
    pxcor <- agents[, 1] + sin(radAngle) * dist
    pycor <- agents[, 2] + cos(radAngle) * dist
    pDistHead <- patch(world = world, x = pxcor, y = pycor, torus = torus,
                       duplicate = TRUE, out = TRUE)

    return(pDistHead)
})

################################################################################
#' All the \code{patches} in a \code{world}
#'
#' Report the coordinates of all the \code{patches} in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the \code{patches} coordinates. The order of the \code{patches}
#'         follows the order of the cells numbers as defined for a \code{Raster*} object.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patches}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' allPatches <- patches(world = w1)
#' NLcount(allPatches) # 100 patches
#'
#' @author Sarah Bauduin
#' @export
#' @rdname patches
setGeneric(
  "patches",
  function(world) {
    standardGeneric("patches")
})

#' @export
#' @rdname patches
setMethod(
  "patches",
  signature = "worldNLR",
  definition = function(world) {
    return(world@pCoords)
})

################################################################################
#' \code{Patch} set
#'
#' Report the \code{patch} coordinates of all the unique \code{patches} contained in the inputs.
#'
#' @param ... Matrices (\code{ncol} = 2) of \code{patches} coordinates with the first column
#'            \code{pxcor} and the second column \code{pycor}.
#'
#' @return Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the \code{patches} coordinates.
#'
#' @details Duplicate \code{patches} among the inputs are removed in the returned matrix.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-set}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- patchAt(world = w1, agents = patch(w1, c(0,1,2), c(0,0,0)), dx = 1, dy = 1)
#' p2 <- patchDistDir(world = w1, agents = patch(w1, 0, 0), dist = 1, angle = 45)
#' p3 <- patch(world = w1, x = 4.3, y = 8)
#' p4 <- patchSet(p1, p2, p3)
#'
#'
#' @export
#' @rdname patchSet
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchSet",
  function(...) {
    standardGeneric("patchSet")
})

#' @export
#' @rdname patchSet
setMethod(
  "patchSet",
  signature = "matrix",
  definition = function(...) {

    dots <- list(...)
    pCoords <- unique(do.call(rbind, dots))
    return(pCoords)
})

################################################################################
#' Random \code{pxcor}
#'
#' Report \code{n} random \code{pxcor} coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Integer. Vector of length \code{n} of \code{pxcor} coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pxcor <- randomPxcor(world = w1, n = 10)
#'
#'
#' @export
#' @rdname randomPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomPxcor",
  function(world, n) {
    standardGeneric("randomPxcor")
})

#' @export
#' @rdname randomPxcor
setMethod(
  "randomPxcor",
  signature = c("worldNLR", "numeric"),
  definition = function(world, n) {
    pxcor <- sample(minPxcor(world):maxPxcor(world), size = n, replace = TRUE)
    return(pxcor)
})

################################################################################
#' Random \code{pycor}
#'
#' Report \code{n} random \code{pycor} coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Integer. Vector of length \code{n} of \code{pycor} coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pycor <- randomPycor(world = w1, n = 10)
#'
#'
#' @export
#' @rdname randomPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomPycor",
  function(world, n) {
    standardGeneric("randomPycor")
})

#' @export
#' @rdname randomPycor
setMethod(
  "randomPycor",
  signature = c("worldNLR", "numeric"),
  definition = function(world, n) {
    pycor <- sample(minPycor(world):maxPycor(world), size = n, replace = TRUE)
    return(pycor)
})
