#' R6 Class representing a TPEGrid grid
#'
#' @description
#' Grid object summarizing information and simulations on the different
#' \code{GridPoints}
#'
#' @usage
#' res <- TPEase$new(...)
#' res$createTpe("TPE1")
#' res$createGrid(tpeID=1, "Grid1", varID, latres, lonres, cols, rows, lon, lat,
#'                multigrid)
#'
#' @details
#' The \code{TPEGrid} object contains all information of the grid as well as a
#' collection of \code{GridPoints}. The object summarizes simulations in all
#' environments (\code{GridPoints}) for a given variety (\code{TPEaseVar})
#'
#' @import R6
#' @export
TPEGrid <- R6::R6Class("TPEGrid",
  public = list(
    #' @field name The identifier of the \code{TPEGrid}
    name = NULL,
    #' @field latRes Latitude resolution of the grid
    latRes = NULL,
    #' @field lonRes Longitude resolution of the grid
    lonRes = NULL,
    #' @field lonStart Starting longitude of the grid (in decimal degrees)
    lonStart = NULL,
    #' @field latStart Starting latitude of the grid (in decimal degrees)
    latStart = NULL,
    #' @field gridPoints A \code{matrix} of \code{GridPoint} objects
    gridPoints = NULL,
    #' @field parameters A \code{matrix} of all parameters
    #' (variety x environment) for each \code{GridPoint}
    parameters = data.frame(),
    #' @field gridRes Summary of simulation results at the grid level
    gridRes = NULL,
    #' @field parent The \code{TPEaseTPE} parent object of \code{TPEGrid}
    parent = NULL,
    #' @field variety A \code{TPEaseVar} to which this grid is linked to
    variety = NULL,

    #' @description Create a new \code{TPEGrid} object
    #' @param name A \code{character} string identifier of the \code{TPEGrid}
    #' @param varID A \code{TPEaseVar} identifier to link to this grid
    #' @param latres A \code{numeric} value of the latitude resolution of the
    #' grid (in decimal degrees)
    #' @param lonres A \code{numeric} value of the longitude resolution of the
    #' grid (in decimal degrees)
    #' @param width A \code{numeric} value of the Width of the grid
    #' @param length A \code{numeric} value of the length of the grid
    #' @param lon A \code{numeric} value of the starting longitude of the
    #' grid (upper left corner)
    #' @param lat A \code{numeric} value of the starting latitude of the grid
    #' (upper left corner)
    #' @param parent The \code{TPEaseTPE} parent object
    #' @return A new `TPEGrid` object.
    initialize = function(name="grid1", varID=NA, latres=0.35, lonres=0.5,
                          width=5, length=5, lon=NA, lat=NA, parent=NA) {
      self$name <- as.character(name)
      if(!is.na(width) && is.numeric(width)) {
        private$width <- width
      }
      if(!is.na(length) && is.numeric(length)) {
        private$length <- length
      }

      if(!is.na(latres) && is.numeric(latres)) {
        self$latRes <- latres
      }

      if(!is.na(lonres) && is.numeric(lonres)) {
        self$lonRes <- lonres
      }

      if(!is.na(lonres) && is.numeric(lonres)) {
        self$lonStart <- lon
      }
      if(!is.na(lonres) && is.numeric(lonres)) {
        self$latStart <- lat
      }

      self$parent <- parent
      self$variety <- varID

      self$gridPoints <- matrix(list(), nrow=length, ncol=width)
      self$parameters <- matrix(list(), nrow=length, ncol=width)
      if(!is.na(lon) & !is.na(lat) & !is.na(latres) & !is.na(lonres)) {
        self$populateGrid()
      } else {
        varNames <- c("Latitude","Longitude","Lat Resolution","Lon Resolution")
        missings <- varNames[is.na(c(lat, lon, latres, lonres))]
        warning(paste("Missing [", paste0(missings, collapse=", "), "]",
                      "grid will not be populated.",
                      "If needed, please use populateGrid() on grid",self$name),
                call.=F)
      }

      self$initMessage()
    },

    #' @description Confirm creation of \code{TPEGrid} object
    initMessage = function() {
      cat(paste0("Grid ", self$name, " of size ", self$get_width(), "x",
                 self$get_length(), " created\n"))
    },

    #' @description Set variety
    #' @param val \code{TPEaseVar} object
    set_var = function(val) {
      if(class(val)[1] == "TPEvar") {
        self$variety <- val
      } else {
        stop(paste0("Error. Trying to set a non variety object into ",
                    self$name))
      }
    },

    #' @description Set \code{TPEGrid} name
    #' @param val New \code{TPEGrid} name
    set_name = function(val) {
      self$name <- as.character(val)
    },

    #' @description Set grid result
    #' @param val A \code{data.frame} of simulation results
    set_gridres = function(val) {
      self$gridRes <- as.data.frame(val)
    },

    ## Getters

    #' @description Get grid width
    get_width = function() {
      return(private$width)
    },

    #' @description Get grid length
    get_length = function() {
      return(private$length)
    },

    #' @description Get grid simulation results
    #' @param traitList A \code{vector} of variable names to extract
    #' from simulations
    get_results = function(traitList = NA) {
      resDf <- data.frame()
      for(i in 1:nrow(self$gridPoints)) {
        for(j in 1:ncol(self$gridPoints)) {
          resDf[nrow(resDf)+1,"x"] <- self$gridPoints[i,j][[1]]$lon
          resDf[nrow(resDf),"y"] <- self$gridPoints[i,j][[1]]$lat
          for(t in traitList) {
            resDf[nrow(resDf),t] <- self$gridPoints[i,j][[1]]$get_sim(t)
          }
        }
      }
      return(resDf)
    },

    #' @description Add result of a \code{GridPoint} to \code{gridRes}
    #' @param val A \code{data.frame} of \code{GridPoint} results
    add_gridpointResult = function(val) {
      self$gridRes <- rbind(self$gridRes,val)
      rownames(self$gridRes) <- NULL
    },

    #' @description Create \code{GridPoint} for each point of the grid
    #' @param lat A \code{numeric} value of the starting latitude of the grid
    #' (upper left corner)
    #' @param lon A \code{numeric} value of the starting longitude of the grid
    #' (upper left corner)
    populateGrid = function(lat=NA, lon=NA) {
      if(!is.na(lat)) {
        self$latStart <- lat
      }
      if(!is.na(lon)) {
        self$lonStart <- lon
      }

      for(i in 1:nrow(self$gridPoints)) {
        latPoint <- self$latStart + (self$latRes*(i-1))
        latName <- sprintf("%02d", i)
        for(j in 1:ncol(self$gridPoints)) {
          lonPoint <- self$lonStart + (self$lonRes*(j-1))
          lonName <- sprintf("%02d", j)
          self$gridPoints[i,j] <-
            list(GridPoint$new(parent=self, name=paste0(latName, lonName),
                               lon=lonPoint, lat=latPoint))
        }
      }
    },

    #' @description Generate climate data for each point of the grid
    #' @param rcp A \code{character} string with the name of the Representative
    #' Concentration Pathway to use. One of the following options
    #' c("rcp26","rcp45","rcp60","rcp85")
    #' @param year A \code{numeric} value of the year to simulate climate
    #' (this can include years from 2013 to 2099)
    #' @param yearNb A \code{numeric} value of the number of years to simulate
    #' @param modelNb A \code{character} string of the  general circulation
    #' model identifier to use, see \code{generateClimate}
    #' @param path A \code{character} string with the path to the marksim
    #' standalone. This path can not contain spaces
    #' @param pathCLI Optional. A \code{character} string with the path to
    #' CLI folder. If no value is provided, the CLI folder will be considered
    #' in the same folder as the marksim standalone. This path can not
    #' contain spaces
    #' @param filesE A \code{boolean} indicating if weather files already exist
    #' @param verbose A \code{boolean} indicating ff messages about starting
    #' climate generation should be shown
    #' @param seed An \code{integer} number to use as seed for marksim weather
    #' generator
    #' @importFrom data.table fread
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI, filesE,
                          verbose, seed) {
      if(filesE) {
        for(i in 1:nrow(self$gridPoints)) {
          for(j in 1:ncol(self$gridPoints)) {
            latF <- self$gridPoints[i,j][[1]]$lat
            lonF <- self$gridPoints[i,j][[1]]$lon
            if(file.size(paste0(path,"weathers/weather_",latF,"_",
                                lonF,".csv")) > 5) {
              weatherF <- data.frame(fread(paste0(path,"weathers/weather_",
                                                  latF,"_", lonF,".csv"),
                                           drop=1))
              self$gridPoints[i,j][[1]]$set_weather(weatherF)
            }
          }
        }
      } else {
        currentPath <- getwd()
        setwd(path)
        cnt <- 0
        nbGP <- nrow(self$gridPoints) * ncol(self$gridPoints)
        pbClim <- txtProgressBar(min = 0, max = nbGP, style = 3)
        for(i in 1:nrow(self$gridPoints)) {
          for(j in 1:ncol(self$gridPoints)) {
            self$gridPoints[i,j][[1]]$genClimate(rcp, year, yearNb, modelNb,
                                                 path, pathCLI, seed)
            cnt <- cnt + 1
            setTxtProgressBar(pbClim, cnt)
          }
        }
        cat("\n")
        setwd(currentPath)
      }
    },

    #' @description Run simulation for each point of the grid
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. A \code{vector} of trait names to extract
    #' from simulations. This will delete the simulations and only keep the
    #' mean of the yearly maximum for each trait
    #' @param savePath Optional. A \code{character} string of the path where
    #' to save simulation files. If \code{NULL} (default),
    #' will not save simulations
    runGridSim = function(soilData, latlonData, cumulP, traitList, savePath) {
      if(is.null(self$variety)) {
        warning(paste("Grid", self$name, "has no variety attached to it",
                      "please provide a varID when calling runGridSim()"))
      }
      cat(paste("\nStarting simulation on", self$name, "grid.",
            "This might take a while depending on the size of the grid \n"))
      cnt <- 1
      nbGP <- nrow(self$gridPoints) * ncol(self$gridPoints)
      pbSim <- txtProgressBar(min = 0, max = nbGP, style = 3)
      for(i in 1:nrow(self$gridPoints)) {
        for(j in 1:ncol(self$gridPoints)) {
          self$gridPoints[i,j][[1]]$runSimulation(self$variety$parameters, i,j,
                                                  soilData, latlonData, cumulP,
                                                  traitList, savePath)
          setTxtProgressBar(pbSim, cnt)
          cnt <- cnt + 1
        }
      }
      cat("\n")
    }
  ),

  private = list(
    width = NULL,
    length = NULL
  )
)
