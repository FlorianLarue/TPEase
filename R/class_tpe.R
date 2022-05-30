#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
TPEa <- R6::R6Class("TPEa",
  public = list(
    #' @field name A character string identifier of the TPE analysis
    name = NULL,
    #' @field model A character string with the name of the crop model
    #' used for simulations (only "Samara" is supported for now)
    model = NULL,
    #' @field varieties A vector of varieties names
    varieties = c(),
    #' @field environments A vector of environment names used for calibration
    environments = c(),
    #' @field genotypes A vector of alternate varieties names
    genotypes = c(),
    #' @field grids A vector of simulation grids, will be populated by the
    #' `createGrid()` function
    grids = NULL,
    #' @field parameters A vector or dataframe with all parameters used for
    #' simulation with the corresponding `model`
    parameters = NULL,
    #' @field weathers A dataframe or list of dataframe with weather data for
    #' each of the `environments`
    weathers = NULL,
    #' @field observations A dataframe or list of dataframe with observations
    #' for each of the `environments`
    observations = NULL,
    #' @field estimParam A vector containing the values of estimated parameters
    #' will be populated by the `runEstimation` function
    estimParam = NULL,
    #' @field maps A list of raster maps for each of the `grids`
    maps = list(),

    #' @description Create a new TPE analysis object
    #' @param name A character string identifier of the TPE analysis
    #' @param model A character string with the name of the crop model used
    #' for simulations
    #' @param varieties A list of varieties names
    #' @param environments A list of environments names
    #' @param genotypes Optional. A list of alternate variety names
    #' @param parameters A vector or dataframe with all crop model parameters
    #' @return A new `TPEa` object.
    initialize = function(name="TPEa_1", model="Samara", varieties=NA,
                          environments=NA, genotypes=NA,parameters=NA) {
      self$name <- as.character(name)
      self$model <- as.character(model)
      if(length(varieties) > 1 || !is.na(varieties)) {
        self$varieties <- as.character(varieties)
      } else {
        stop("Please provide at least one variety name.")
      }
      if(length(environments) > 1 || !is.na(environments)) {
        self$environments <- environments
      } else {
        stop("Please provide at least one environment name.")
      }
      if(!is.na(genotypes)) {
        if(length(genotypes != length(varieties))) {
          stop(paste("Length of genotypes does not match length of varieties.",
               "Please either provide an alternate name for each variety",
               "or use genotypes=NA."))
        } else {
          self$genotypes <- genotypes
        }
      } else {
        self$genotypes <- self$varieties
      }
      if(length(varieties) > 1 || !is.na(parameters)) {
        self$parameters <- parameters
      } else {
        warning(paste("No parameters were provided",
                      "estimation and grid simulation will not be possible.",
                      "You can set parameters on this TPE analysis",
                      "by running set_param()."))
      }
      self$initMessage()
    },

    #' @description Set crop model
    #' @param val New model name
    set_model = function(val) {
      self$model <- val
    },

    #' @description Set weather data
    #' @param val A dataframe or list of dataframes with weather data for each
    #' of the `environments`
    set_weather = function(val) {
      self$weathers <- val
    },

    #' @description Set observation data
    #' @param val A dataframe or list of dataframes with observations for each
    #' of the `environments` and `varieties`
    set_obs = function(val) {
      self$observations <- val
    },

    #' @description Set parameters
    #' @param val A vector or dataframe with all crop model parameters
    set_param = function(val) {
      self$parameters <- val
    },

    #' @description Confirm creation of TPE analysis object
    initMessage = function() {
      plV <- length(self$varieties) > 1
      plE <- length(self$environmets) > 1
      cat(paste0("TPE analysis ", self$name, " created containing ",
                 length(self$varieties), ifelse(plV," varieties "," variety "),
                 "and ", length(self$environments), ifelse(plE," environments",
                                                           " environment. \n")))
    },

    #' @description Create a simulation grid
    #' @param name A character string identifier of the grid
    #' @param res A numeric value of the resolution of the grid
    #' @param cols A numeric value of the number of columns in the grid
    #' @param rows A numeric value of the number of rows in the grid
    #' @param lon A numeric value of the starting longitude of the grid in
    #' decimal degrees
    #' @param lat A numeric value of the starting latitude of the grid in
    #' decimal degrees
    createGrid = function(name="grid1", res=0.5, cols=5, rows=5, lon=NA, lat=NA) {
        if(name %in% private$gridnames) {
        stop(paste("Grid with name", name,"already exists.",
                   "Please provide a unique identifier."))
      }
      self$grids[[length(self$grids)+1]] <- TPEgrid$new(name, res, cols, rows,
                                                      lon, lat, self)
      private$gridnames <- c(private$gridnames, name)
    },

    #' @description Generate climate data for one or several grids
    #' @param gridID A vector of grid identifiers (either index or name)
    #' @param rcp A character string with the name of the Representative
    #' Concentration Pathway to use. One of the following options
    #' c("rcp26","rcp45","rcp60","rcp85")
    #' @param year A numeric value of the year to simulate climate
    #' (this can include years from 2013 to 2099)
    #' @param yearNb A numeric value of the number of years to simulate
    #' @param modelNb A character string of the  general circulation model
    #' identifier to use, see \code{generateClimate}
    #' @param path A character string with the path to the marksim standalone.
    #' For the moment, this path can not contain spaces
    #' @param pathCLI Optional. A character string with the path to CLI folder.
    #' If no value is provided, the CLI folder will be considered in the same
    #' folder as the marksim standalone. For the moment, this path can not
    #' contain spaces
    genClimate = function(gridID=1, rcp="rcp26", year=2014, yearNb=1,
                       modelNb="00000000000000000", path=NA, pathCLI=NA) {
      for(i in 1:length(gridID)) {
        cat(paste("Generating climate for grid",gridID, "this may take some",
                  "time if this is the first time generating climate",
                  "for this grid.\n"))
        if(class(gridID) == "numeric") {
          id <- gridID[[i]]
        } else {
          id <- match(gridID[[i]], private$gridnames)
        }
        self$grids[[id]]$genClimate(rcp, year, yearNb, modelNb, path, pathCLI)
      }
    },

    #' @description Run simulation on one or several grids
    #' @param gridID A vector of grid identifiers (either index or name)
    #' @param row A numeric value with the row of parameter dataframe to use
    #' @param trait A character string with the trait name for grid res matrix
    #' @param year A numeric value with the year to run the simulation
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    runGridSim = function(gridID=1, row=1, trait="GrainYieldPopFin",
                          year=2015, soilData=soil, latlonData=lat_lon) {
      param <- self$parameters[row,]
      for(i in 1:length(gridID)) {
        if(class(gridID) == "numeric") {
          id <- gridID[[i]]
        } else {
          id <- match(gridID[[i]], private$gridnames)
        }
        self$grids[[id]]$runGridSim(trait, year, soilData, latlonData, param)
      }
    },

    #' @description Create raster map
    #' @param res A numeric valuein sec of the resolution of world map to use.
    #' Options are c(1, 150, 900) for respectively 30sec, 2.5min, 15min
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @importFrom raster raster
    #' @importFrom raster crop
    #' @importFrom raster extent
    #' @importFrom raster crs
    createMap = function(res=150, bounds=NA) {
      tmpmap <- raster(paste0("data/gis/world_",as.character(res),".tif"))
      if(length(bounds) == 4) {
        e <- as(raster::extent(bounds[4],bounds[2],bounds[3],bounds[1]),
                "SpatialPolygons")
        raster::crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
        tmpmap <- crop(tmpmap, e)
      } else if(!is.na(bounds)) {
          stop(paste("Length of bounds is not 4.",
                     "Please provide a value for each cardinal point,",
                     "or use bounds=NA"))
      }
        map <- as(tmpmap, "SpatialPixelsDataFrame")
        map <- as.data.frame(map)
        colnames(map) <- c("value", "x", "y")
        map$value <- NA
        self$maps[[length(self$maps)+1]] <- map
    },

    #' @description Create plot on map
    #' @param mapID A numeric value of the index of map to plot
    #' @param gridID A vector of grid identifiers (either index or name)
    plotMap = function(mapID=1, gridID=1) {
      for(i in 1:length(gridID)) {
        if(class(gridID) == "numeric") {
          id <- gridID[[i]]
        } else {
          id <- match(gridID[[i]], private$gridnames)
        }
        self$grids[[id]]$plotMap(mapID=mapID)
      }
    },

    #' @description Run parameter estimation
    #' @param maxiter A numeric value of the maximum number of iteration
    #' for DEoptim
    #' @param paramnames A vector of parameter names to be estimated
    #' @param metric A character string with the name of the metric to use
    #' for fitness computation. Options are c("RMSE","MAE","MSE")
    #' @param score_fn A function to compute fitness, see \code{get_score}
    #' @param weigh_fn Not used for the moment
    #' @import rsamara
    #' @import DEoptim
    runEstimation = function(maxiter=2000,paramnames=NA, metric="RMSE",
                             score_fn=get_score, weigh_fn=NA) {

      DEParams <- DEoptim.control(itermax=maxiter,strategy=2,trace=1,
                                  NP=10*length(paramOfInterest))

      resEstim <- DEoptim::DEoptim(estim_param, paramBounds[,1],
                                   paramBounds[,2], control=DEParams,
                                   self$environments, self$parameters,
                                   paramnames, self$weathers, self$observations,
                                   score_fn, metric, weigh_fn,
                                   fnMap=NULL)
      private$set_eparam(as.vector(resEstim$optim$bestmem))
      private$update_param(self$estimParam, paramnames)
    }
  ),
  private = list(
    gridnames = NULL,

    set_eparam = function(val) {
      self$estimParam <- val
    },

    update_param = function(val,names) {
      for(p in 1:length(names)) {
        self$parameters[,which(colnames(self$parameters) ==
                                 names[[p]])] <- val[[p]]
      }
    }
  )
)
