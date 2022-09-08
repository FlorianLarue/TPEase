#'#' R6 Class Representing a simulation grid
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
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
    #' @field variety Variety to which this grid is linked to
    variety = NULL,
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE grid object
    #' @param name Identifier of the grid
    #' @param varID Identifier of the variety to which this grid is linked
    #' @param res Resolution of the grid
    #' @param width Width of the grid
    #' @param length Length of the grid
    #' @param lon Optional. Starting longitude of the grid (upper left corner)
    #' @param lat Optional. Starting latitude of the grid (upper left corner)
    #' @param parent TPE analysis parent
    #' @return A new `TPEgrid` object.
    initialize = function(name="grid1", varID=NA, res=5, width=5, length=5,
                          lon=NA, lat=NA, parent=NA) {
      self$name <- name
      self$width <- width
      self$length <- length
      self$res <- res
      self$lonStart <- lon
      self$latStart <- lat
      self$parent <- parent
      self$variety <- varID

      #TODO: might have to find a better solution than having list in matrix
      #TODO: need to generalize (see improvements.md)
      #tmp solution for Adam et al.
      self$gridPoints <- matrix(list(), nrow=length, ncol=width)
      self$resGrid <- matrix(NA, nrow=length, ncol=width)
      if(!is.na(lon) & !is.na(lat)) {
        for(i in 1:nrow(self$gridPoints)) {
          latPoint <- self$latStart + (0.35*(i-1))
          if(i < 10) {
            ii <- paste0("0",i)
          } else {
            ii <- i
          }
          for(j in 1:ncol(self$gridPoints)) {
            lonPoint <- self$lonStart + (res*(j-1))
            if(j < 10) {
              jj <- paste0("0",j)
            } else {
              jj <- j
            }
            self$gridPoints[i,j] <- list(gridPoint$new(parent=self,
                                                       name=paste0(ii,jj),
                                                       lon=lonPoint,
                                                       lat=latPoint))
          }
        }
      } else {
        warning(paste("Missing longitude or latitude",
                      "grid will not be populated. If needed,",
                      "please use populateGrid() on grid",self$name),
                call.=F)
      }
    },

    #' @description Set simulation result for given grid point in result matrix
    #' @param i Row position of grid point
    #' @param j Col position of grid point
    #' @param val Value of simulation result
    set_resGrid = function(i, j, val) {
      self$resGrid[i,j] <- val
    },

    #' @description Set variety
    #' @param val Variety
    set_var = function(val) {
      if(class(val)[1] == "TPEvar") {
        self$gridVar <- val
      } else {
        stop(paste0("Error. Trying to set a non variety object into ",
                    self$name))
      }
    },

    #' @description Generate climate data for each point of the grid
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    #' @param filesE Boolean. If weather files already exist
    #' @param verbose Boolean. If messages about completing climate generation
    #' should be shown
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI, filesE,
                          verbose) {
      cnt <- 1
      if(filesE) { #tmp solution for dev
        for(i in 1:nrow(self$gridPoints)) {
          for(j in 1:ncol(self$gridPoints)) {
            latF <- self$gridPoints[i,j][[1]]$lat
            lonF <- self$gridPoints[i,j][[1]]$lon
            if(file.size(paste0(path,"weathers/weather_",latF,"_",
                                lonF,".csv")) > 5) {
              weatherF <- read.csv(paste0(path,"weathers/weather_",latF,"_",
                                          lonF,".csv"), row.names=1) #tmp
              self$gridPoints[i,j][[1]]$weather <- weatherF
              if(verbose) {
                print(paste("Climate of point", cnt, "out of",
                            as.numeric(self$width) * as.numeric(self$length),
                            "generated"))
              }
              cnt <- cnt + 1
            }
          }
        }
      } else {
        currentPath <- getwd()
        setwd(path)
        nbGP <- nrow(self$gridPoints) * ncol(self$gridPoints)
        pbClim <- txtProgressBar(min = 0, max = nbGP, style = 3)
        for(i in 1:nrow(self$gridPoints)) {
          for(j in 1:ncol(self$gridPoints)) {
            self$gridPoints[i,j][[1]]$genClimate(rcp, year, yearNb, modelNb,
                                                 path, pathCLI)
            if(verbose) {
              print(paste("Climate of point", cnt, "out of",
                          as.numeric(self$width) * as.numeric(self$length),
                          "generated"))
            }
            setTxtProgressBar(pbClim, cnt)
            cnt <- cnt + 1
          }
        }
        setwd(currentPath)
      }
    },
    #' @description Run simulation for each point of the grid
    #' @param trait Trait name for grid res matrix
    #' @param year Year to run the simulation
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    runGridSim = function(trait, year, soilData, latlonData) {
      if(is.na(self$gridVar)) {
        warning(paste("Grid", self$name, "has no variety attached to it",
                      "please provide a varID when calling runGridSim()"))
      }
      cat(paste("Starting simulation on", self$name, "grid.",
            "This might take a while depending on the size of the grid \n"))
      cnt <- 1
      nbGP <- nrow(self$gridPoints) * ncol(self$gridPoints)
      pbSim <- txtProgressBar(min = 0, max = nbGP, style = 3)
      for(i in 1:nrow(self$gridPoints)) {
        for(j in 1:ncol(self$gridPoints)) {
          self$gridPoints[i,j][[1]]$set_soilParam(soilData, latlonData)
          self$gridPoints[i,j][[1]]$set_dateParam(year)
          self$gridPoints[i,j][[1]]$runSimulation(self$gridVar$parameters,
                                                  trait, i,j)
          setTxtProgressBar(pbSim, cnt)
          cnt <- cnt + 1
        }
      }
    }
  )
)
