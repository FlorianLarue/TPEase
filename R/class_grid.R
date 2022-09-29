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
    #' @field latRes Latitude resolution of the grid
    latRes = NULL,
    #' @field lonRes Longitude resolution of the grid
    lonRes = NULL,
    #' @field lonStart Starting longitude of the grid
    lonStart = NULL,
    #' @field latStart Starting latitude of the grid
    latStart = NULL,
    #' @field gridPoints Collection of each point on the grid
    gridPoints = NULL,
    #' @field gridRes Result dataframe (constructed during map clustering)
    gridRes = NULL,
    #' @field parent TPE analysis parent
    parent = NULL,
    #' @field variety Variety to which this grid is linked to
    variety = NULL,
    #' @field test Debug
    test = NULL,

    #' @description Create a new TPE grid object
    #' @param name Identifier of the grid
    #' @param varID Identifier of the variety to which this grid is linked
    #' @param latres Latitude resolution of the grid (in decimal degrees)
    #' @param lonres Longitude resolution of the grid (in decimal degrees)
    #' @param width Width of the grid
    #' @param length Length of the grid
    #' @param lon Optional. Starting longitude of the grid (upper left corner)
    #' @param lat Optional. Starting latitude of the grid (upper left corner)
    #' @param parent TPE analysis parent
    #' @return A new `TPEgrid` object.
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

      #TODO: might have to find a better solution than having list in matrix
      self$gridPoints <- matrix(list(), nrow=length, ncol=width)
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

    #' @description Confirm creation of Grid object
    initMessage = function() {
      cat(paste0("Grid ", self$name, " of size ", self$get_width(), "x",
                 self$get_length(), " created\n"))
    },

    ## Setters

    #' @description Set variety
    #' @param val Variety
    set_var = function(val) {
      if(class(val)[1] == "TPEvar") {
        self$variety <- val
      } else {
        stop(paste0("Error. Trying to set a non variety object into ",
                    self$name))
      }
    },

    #' @description Set grid name
    #' @param val Variety
    set_name = function(val) {
      self$name <- as.character(val)
    },

    #' @description Set grid result dataframe
    #' @param val Dataframe
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
    #' @param traitList Vector of variable names to extract for results
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

    #' @description Create gridpoint for each point of the grid
    #' @param lat Starting latitude of the grid (upper left corner)
    #' @param lon Starting longitude of the grid (upper left corner)
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
          self$gridPoints[i,j] <- list(gridPoint$new(parent=self,
                                                     name=paste0(latName,
                                                                 lonName),
                                                     lon=lonPoint,
                                                     lat=latPoint))
        }
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
    #' @param seed Integer number to use as seed for marksim weather generator
    #' @importFrom data.table fread
    #' @importFrom data.table fwrite
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI, filesE,
                          verbose, seed) {
      if(filesE) { #TODO: tmp solution for dev, need to find generalization
        for(i in 1:nrow(self$gridPoints)) {
          for(j in 1:ncol(self$gridPoints)) {
            latF <- self$gridPoints[i,j][[1]]$lat
            lonF <- self$gridPoints[i,j][[1]]$lon
            if(file.size(paste0(path,"weathers/weather_",latF,"_",
                                lonF,".csv")) > 5) {
              weatherF <- data.frame(data.table::fread(paste0(path,
                                                      "weathers/weather_",
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
    #' @param traitList Optionnal. Vector of trait names to extract from
    #' simulations. This will delete the simulations and only keep the mean of
    #' the yearly maximum for each trait
    #' @param savePath Optional. A character string of the path where to save
    #' simulation files. If NULL (default), will not save simulations
    runGridSim = function(soilData, latlonData, traitList, savePath) {
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
                                                  soilData, latlonData,
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
