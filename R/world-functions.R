################################################################################
#' Maximum \code{pxcor}
#'
#' Report the patches maximum \code{pxcor} in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' maxPxcor(w1)
#'
#'
#' @export
#' @rdname maxPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPxcor",
  function(world) {
    standardGeneric("maxPxcor")
  })

#' @export
#' @rdname maxPxcor
setMethod(
  "maxPxcor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@maxPxcor)
  }
)


################################################################################
#' Maximum \code{pycor}
#'
#' Report the patches maximum \code{pycor} in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' maxPycor(w1)
#'
#'
#' @export
#' @rdname maxPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPycor",
  function(world) {
    standardGeneric("maxPycor")
  })

#' @export
#' @rdname maxPycor
setMethod(
  "maxPycor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@maxPycor)
  }
)


################################################################################
#' Minimum \code{pxcor}
#'
#' Report the patches minimum \code{pxcor} in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' minPxcor(w1)
#'
#'
#' @export
#' @rdname minPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minPxcor",
  function(world) {
    standardGeneric("minPxcor")
  })

#' @export
#' @rdname minPxcor
setMethod(
  "minPxcor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@minPxcor)
  }
)


################################################################################
#' Minimum \code{pycor}
#'
#' Report the patches minimum \code{pycor} in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' minPycor(w1)
#'
#'
#' @export
#' @rdname minPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minPycor",
  function(world) {
    standardGeneric("minPycor")
  })

#' @export
#' @rdname minPycor
setMethod(
  "minPycor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@minPycor)
  }
)


################################################################################
#' \code{World} width
#'
#' Report the width of the \code{world} in \code{patch} number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' worldWidth(w1)
#'
#'
#' @export
#' @rdname worldWidth
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "worldWidth",
  function(world) {
    standardGeneric("worldWidth")
  })

#' @export
#' @rdname worldWidth
setMethod(
  "worldWidth",
  signature = "worldNLR",
  definition = function(world) {
    wWidth <- maxPxcor(world) - minPxcor(world) + 1
    return(wWidth)
  }
)


################################################################################
#' \code{World} height
#'
#' Report the height of the \code{world} in \code{patch} number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' worldHeight(w1)
#'
#'
#' @export
#' @rdname worldHeight
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "worldHeight",
  function(world) {
    standardGeneric("worldHeight")
  })

#' @export
#' @rdname worldHeight
setMethod(
  "worldHeight",
  signature = "worldNLR",
  definition = function(world) {
    wHeight <- maxPycor(world) - minPycor(world) + 1
    return(wHeight)
  }
)


################################################################################
#' Clear \code{world}'s \code{patches}
#'
#' Reset all \code{patches} values to \code{NA}.
#'
#' @inheritParams fargs
#'
#' @return \code{WorldMatrix} object with \code{NA} values for all \code{patches}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-patches}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' w1 <- NLset(world = w1, agents = patches(w1), val = runif(NLcount(patches(w1))))
#' w1Val <- of(world = w1, agents = patches(w1))
#' summary(w1Val)
#'
#' w1 <- clearPatches(w1)
#' w1Val <- of(world = w1, agents = patches(w1))
#' summary(w1Val)
#'
#'
#' @export
#' @rdname clearPatches
#'
#' @author Sarah Bauduin
#'
#' @include worldNLR-classes-methods.R
setGeneric(
  "clearPatches",
  function(world) {
    standardGeneric("clearPatches")
  }
)

#' @export
#' @rdname clearPatches
setMethod(
  "clearPatches",
  signature = c("worldMatrix"),
  definition = function(world) {
    world@.Data[] <- NA
    return(world)
  }
)

#' @export
#' @rdname clearPatches
setMethod(
  "clearPatches",
  signature = c("worldArray"),
  definition = function(world) {
    worldNA <- createWorld(minPxcor = minPxcor(world), maxPxcor = maxPxcor(world),
                                   minPycor = minPycor(world), maxPycor = maxPycor(world))
    return(worldNA)
  }
)


################################################################################
#' Convert a \code{Raster*} object into a \code{worldMatrix} or \code{worldArray} object
#'
#' Convert a \code{RasterLayer} object into a \code{worldMatrix} object or a \code{RasterStack} object
#' into a \code{worldArray} object.
#'
#' @param raster \code{RasterLayer} or \code{RasterStack} object.
#'
#' @param method "\code{ngb} or \code{bilinear} for the resample method.
#'
#' @return \code{WorldMatrix} or \code{worldArray} object depending on the input \code{raster}.
#'         \code{Patches} value are retained from the \code{raster}.
#'
#' @details See \code{help("worldMatrix-class")} or \code{help("worldArray-class")}
#'          for more details on the classes.
#'
#'          The \code{raster} is resampled to match the coordinates system and
#'          resolution of a \code{worldMatrix} or \code{worldArray} using the chosen \code{method}. The
#'          extent will be bigger by 1 on the width and on the height.
#'
#' @examples
#' r1 <- raster(extent(c(0, 10, 0, 10)), nrows = 10, ncols = 10)
#' r1[]<-runif(100)
#' w1 <- raster2world(r1, method = "ngb")
#' plot(r1)
#' plot(w1)
#'
#'
#' @export
#' @importFrom abind abind
#' @rdname raster2world
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "raster2world",
  function(raster, method) {
    standardGeneric("raster2world")
  })


#' @export
#' @rdname raster2world
setMethod(
  "raster2world",
  signature = c("RasterLayer", "character"),
  definition = function(raster, method) {

    minPxcor <- round(raster@extent@xmin)
    maxPxcor <- round(raster@extent@xmax)
    minPycor <- round(raster@extent@ymin)
    maxPycor <- round(raster@extent@ymax)
    world <- createWorld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                                 minPycor = minPycor, maxPycor = maxPycor)
    worldRaster <- raster(world@extent)
    res(worldRaster) <- c(1, 1)

    worldR <- resample(raster, worldRaster, method = method)

    world[] <- values(worldR)

    return(world)
  })


#' @export
#' @rdname raster2world
setMethod(
  "raster2world",
  signature = c("RasterStack", "character"),
  definition = function(raster, method) {

    minPxcor <- round(raster@extent@xmin)
    maxPxcor <- round(raster@extent@xmax)
    minPycor <- round(raster@extent@ymin)
    maxPycor <- round(raster@extent@ymax)
    world <- createWorld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                                 minPycor = minPycor, maxPycor = maxPycor)
    worldRaster <- raster(world@extent)
    res(worldRaster) <- c(1, 1)

    worldR <- lapply(1:nlayers(raster), function(x) {
      layer <- resample(raster[[x]], worldRaster, method = method)
      matrix(values(layer), ncol = dim(world)[2], byrow = TRUE)
    })

    out <- abind::abind(worldR, along = 3)
    dimnames(out) <- list(NULL, NULL, names(raster))

    wArray <- new("worldArray",
                      .Data = out,
                      minPxcor = minPxcor, maxPxcor = maxPxcor,
                      minPycor = minPycor, maxPycor = maxPycor,
                      extent = world@extent,
                      res = c(1, 1),
                      pCoords = world@pCoords
    )

    return(wArray)
  })


################################################################################
#' Convert a \code{worldMatrix} or \code{worldArray} object into a \code{Raster*} object
#'
#' Convert a \code{worldMatrix} object into a \code{RasterLayer} object or a
#' \code{worldArray} object into a \code{RasterStack} object
#'
#' @inheritParams fargs
#'
#' @return \code{RasterLayer} or \code{RasterStack} object depending on the input \code{world}.
#'         \code{Patches} value are retained from the \code{world}.
#'
#' @details The \code{Raster*} returned has the same extent and resolution as the \code{world}
#'          with round coordinates at the center of the cells and coordinates \code{x.5}
#'          at the edges of the cells.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = runif(100))
#' r1 <- world2raster(w1)
#' plot(r1)
#'
#'
#' @export
#' @rdname world2raster
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "world2raster",
  function(world) {
    standardGeneric("world2raster")
  })


#' @export
#' @rdname world2raster
setMethod(
  "world2raster",
  signature = c("worldMatrix"),
  definition = function(world) {
    ras <- raster(world@.Data, xmn = world@extent@xmin, xmx = world@extent@xmax,
                     ymn = world@extent@ymin, ymx = world@extent@ymax)

    return(ras)
  })


#' @export
#' @rdname world2raster
setMethod(
  "world2raster",
  signature = c("worldArray"),
  definition = function(world) {

    listRaster <- lapply(1:dim(world)[3], function(x) {
      raster(world@.Data[, , x], xmn = world@extent@xmin, xmx = world@extent@xmax,
             ymn = world@extent@ymin, ymx = world@extent@ymax)
    })
    rasterStack <- stack(listRaster)
    return(rasterStack)
})

#' Key base R functions for \code{worldNLR} classes
#'
#' Slight modifications from the default versions.
#'
#' @param object  An \code{agentMatrix} object.
#'
#' @export
#' @importFrom quickPlot numLayers
#' @rdname show-methods
setMethod(
  "show",
  signature(object = "worldArray"),
  definition = function(object) {
    cat("class       :", class(object), "\n")
    cat("resolution  :", paste(object@res, collapse = ", "), "(x, y)\n")
    cat("dimensions  : Pxcor: ", object@minPxcor, ",", object@maxPxcor, "\n",
        "             Pycor: ", object@minPycor, ",", object@maxPycor, "\n")

    # Copied and modified from show method in Raster
      minv <- format(apply(object@.Data, 3, min))
      maxv <- format(apply(object@.Data, 3, max))
      minv <- gsub("Inf", "?", minv)
      maxv <- gsub("-Inf", "?", maxv)
      nl <- numLayers(object)
      mnr <- 15

      if (nl > mnr) {
        minv <- c(minv[1:mnr], "...")
        maxv <- c(maxv[1:mnr], "...")
      }

      ln <- dimnames(object)[[3]]
      n <- nchar(ln)
      if (nl > 5) {
        b <- n > 26
        if (any(b)) {
          mid <- floor(n/2)
          ln[b] <- paste(substr(ln[b], 1, 9), "//",
                         substr(ln[b], nchar(ln[b]) - 9, nchar(ln[b])), sep = "")
        }
      }

      w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
      m <- rbind(ln, minv, maxv)
      # a loop because 'width' is not recycled by format
      for (i in 1:ncol(m)) {
        m[, i]   <- format(m[, i], width = w[i], justify = "right")
      }
      cat("names       :", paste(m[1, ], collapse = ", "), "\n")
      cat("min values  :", paste(m[2, ], collapse = ", "), "\n")
      cat("max values  :", paste(m[3, ], collapse = ", "), "\n")

    # } else {
    #   cat("names       :", paste(ln, collapse=", "), "\n")
    # }

    cat("First 4 rows and columns:\n")
    print(object@.Data[1:4,1:4,])
})

#' @export
#' @rdname show-methods
setMethod(
  "show",
  signature(object = "worldMatrix"),
  definition = function(object) {
    cat("class       :", class(object), "\n")
    cat("resolution  :", paste(object@res, collapse = ", "), "(x, y)\n")
    cat("dimensions  : Pxcor: ", object@minPxcor, ",", object@maxPxcor, "\n",
        "             Pycor: ", object@minPycor, ",", object@maxPycor, "\n")

    # Copied and modified from show method in Raster
    minv <- format(min(object@.Data))
    maxv <- format(max(object@.Data))
    minv <- gsub("Inf", "?", minv)
    maxv <- gsub("-Inf", "?", maxv)

    ln <- "layer"
    w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
    m <- rbind(ln, minv, maxv)
    # a loop because 'width' is not recycled by format
    m   <- format(m, width = w, justify = "right")
    cat("names       :", paste(m[1, ], collapse = ", "), "\n")
    cat("min values  :", paste(m[2, ], collapse = ", "), "\n")
    cat("max values  :", paste(m[3, ], collapse = ", "), "\n")

    cat("First 4 rows and columns:\n")
    print(object@.Data[1:4, 1:4])
})
