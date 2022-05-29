#'#' R6 Class Representing a simulation grid
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
TPEgrid <- R6::R6Class("TPEgrid",
  public = list(
    #' @field name Identifier of the grid
    name = NULL,
    #' @field length Length of the grid
    length = NULL,
    #' @field width Width of the grid
    width = NULL,
    #' @field res Resolution of the grid
    res = NULL,
    #' @field lonStart Starting longitude of the grid
    lonStart = NULL,
    #' @field latStart Starting latitude of the grid
    latStart = NULL,
    #' @field gridPoints Collection of each point on the grid
    gridPoints = NULL,
    #' @field resGrid Matrix with value of simulated trait for each grid point
    resGrid = NULL,
    #' @field parent TPE analysis parent
    parent = NULL,

    #' @description Create a new TPE grid object
    #' @param name Identifier of the grid
    #' @param res Resolution of the grid
    #' @param width Width of the grid
    #' @param length Length of the grid
    #' @param lon Optional. Starting longitude of the grid (upper left corner)
    #' @param lat Optional. Starting latitude of the grid (upper left corner)
    #' @param parent TPE analysis parent
    #' @return A new `TPEgrid` object.
    initialize = function(name="grid1", res=5, width=5, length=5,
                          lon=NA, lat=NA, parent=NA) {
      self$name <- name
      self$width <- width
      self$length <- length
      self$res <- res
      self$lonStart <- lon
      self$latStart <- lat
      self$parent <- parent

      #TODO: might have to find a better solution than having list in matrix
      #TODO: need to generalize (see improvements.md)
      #tmp solution for Adam et al.
      self$gridPoints <- matrix(list(), nrow=length, ncol=width)
      self$resGrid <- matrix(NA, nrow=length, ncol=width)
      if(!is.na(lon) & !is.na(lat)) {
        for(i in 1:nrow(self$gridPoints)) {
          latPoint <- self$latStart + (0.35*i)
          for(j in 1:ncol(self$gridPoints)) {
            lonPoint <- self$lonStart + (res*j)
            self$gridPoints[i,j] <- list(gridPoint$new(parent=self,
                                                       name=paste0(i,j),
                                                       lon=lonPoint,
                                                       lat=latPoint))
          }
        }
      } else {
        cat(paste("Missing longitude or latitude, grid will not be populated.",
                  "If needed, please use populateGrid() on grid",self$name))
      }
    },

    #' @description Set simulation result for given grid point in result matrix
    #' @param i Row position of grid point
    #' @param j Col position of grid point
    #' @param val Value of simulation result
    set_resGrid = function(i, j, val) {
      self$resGrid[i,j] <- val
    }
  )
)
