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
    },

    #' @description Generate climate data for each point of the grid
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    genClimate = function(rcp="rcp26", year=2015, yearNb=99,
                       modelNb="00000000000000000", path=NA, pathCLI=NA) {
      for(i in 1:nrow(self$gridPoints)) {
        for(j in 1:ncol(self$gridPoints)) {
          self$gridPoints[i,j][[1]]$genClimate(rcp, year, yearNb, modelNb, path,
                                               pathCLI)
        }
      }
    },
    #' @description Run simulation for each point of the grid
    #' @param trait Trait name for grid res matrix
    #' @param year Year to run the simulation
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param param Parameters to use for simulation
    runGridSim = function(trait="GrainYieldPopFin", year=2015, soilData=soil,
                          latlonData=lat_lon, param) {
      for(i in 1:nrow(self$gridPoints)) {
        for(j in 1:ncol(self$gridPoints)) {
          self$gridPoints[i,j][[1]]$set_soilParam(soil, lat_lon)
          self$gridPoints[i,j][[1]]$set_dateParam(year)
          self$gridPoints[i,j][[1]]$runSimulation(param, trait, i,j)
        }
      }
    }
  )
)
