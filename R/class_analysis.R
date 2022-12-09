#'#' R6 Class Representing a CGMTPE analysis
#'
#' @description
#' TODO: change name
#' The CGMTPE Analysis object "analysis" is an environment containing all
#' objects used for estimation and TPE analysis
#' (varieties, environments, grids, maps, etc.)
#'
#' @details
#' TODO
#' @import R6
#' @export
CGMTPEa <- R6::R6Class("CGMTPEa",
  public = list(
    ## Attributes

    #' @field name A character string identifier of the CGMTPE analysis
    name = NULL,
    #' @field model A character string with the name of the crop model
    #' used for simulations (only "Samara" is supported for now)
    model = NULL,
    #' @field varieties A list of varieties objects
    varieties = list(),
    #' @field TPEanalysis A list of TPE analysis objects
    TPEanalysis = list(),
    #' @field test Debug
    test = NULL,


    ## Methods

    #' @description Create a new TPE analysis object
    #' @param name A character string identifier of the TPE analysis
    #' @param model A character string with the name of the crop model used
    #' for simulations
    #' @param varieties A list of varieties names
    #' @param genotypes Optional. A list of alternate variety names
    #' @param vparameters A vector or dataframe with variety specific parameters
    #' @param environments A list of environments names
    #' @param eparameters A vector or dataframe with environment specific
    #' parameters
    #' @param eName A character string identifier of the initial estimation
    #' object
    #' @param weathers A list (each entry is an environment)
    #'  of dataframes with weather data
    #' @param observations A list (each entry is a variety) of lists
    #' (each entry is an environment) of dataframes with observations
    #' @return A new `CGMTPEa` object.
    initialize = function(name="CGMTPE_analysis", model="Samara", varieties=NA,
                          genotypes=NA, vparameters=NA, environments=NA,
                          eparameters=NA, eName="estim1", weathers=NA,
                          observations=NA) {

      self$name <- as.character(name)
      self$model <- as.character(model)

      vParamMissings <- c()

      if(length(varieties) > 1 || !is.na(varieties)) {
        private$varnames <- as.character(varieties)

        if(length(genotypes) != length(varieties) || is.na(genotypes)) {
          private$genotypes <- as.character(varieties)
          warning(paste("Length of genotypes does not match",
                        "length of varieties, name of varieties will be used."),
                  call.=F)
        } else {
          private$genotypes <- genotypes
        }

        for(i in 1:length(varieties)) {
          #TODO: change name of TPEvar class
          if(class(vparameters) == "data.frame" && nrow(vparameters) >= i) {
            param <- vparameters[i,]
            rownames(param) <- NULL
          } else {
            vParamMissings <- c(vParamMissings, private$varnames[i])
            param <- NULL
          }

          self$varieties <- append(self$varieties, TPEvar$new(
            name = as.character(private$varnames[i]),
            alt = as.character(private$genotypes[i]),
            parameters = param,
            eName = as.character(eName),
            environments = environments,
            eparam = eparameters,
            weathers = weathers,
            observations = observations[[i]],
            parent = self))
        }
      } else {
        stop("Please provide at least one variety name.")
      }

      self$initMessage(vParamMissings)
    },

    #' @description Confirm creation of TPE analysis object
    #' @param vParamMissings Name of varieties without parameters
    initMessage = function(vParamMissings) {
      if(length(vParamMissings) > 0) {
        warning(paste0("No parameters were provided for ",
                       ifelse(length(vParamMissings > 1), "varieties [", "variety ["),
                       paste0(vParamMissings, collapse=" "),
                       "]. Simulations will not be possible. ",
                       "You can set parameters by running set_param() on the ",
                       "variety object."), call.=F)
      }

      cat(paste0("CGMTPE analysis ", self$name, " created containing ",
                 length(private$varnames), ifelse(length(private$varnames) > 1,
                                                  " varieties "," variety "),
                 "\n"))
    },

    #' @description Set TPE analysis name
    #' @param val New TPE analysis name
    set_name = function(val) {
      self$name <- as.character(val)
    },

    #' @description Set model name
    #' @param val New model name
    set_model = function(val) {
      self$model <- as.character(val)
    },

    #' @description Set varieties, this is a list of TPEvar objects,
    #' use with caution
    #' @param val New varieties
    set_var = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEvar") {
          self$varieties <- list(val)
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEvar"))) == 0) {
          self$varieties <- val
        } else {
          stop(paste0("Error. Trying to set a non variety object into ",
                      self$name))
        }
      } else {
        stop("Depth of varieties can not exceed 1")
      }
    },

    #' @description Get names of all varieties
    get_varNames = function() {
      return(private$varnames)
    },

    #' @description Get names of all genotypes
    get_genotypes = function() {
      return(private$genotypes)
    },

    #' @description Get var id
    #' @param val Either var name or a var id (will return the var id)
    get_varid = function(val) {
      if(class(val) == "numeric") {
        if(val > length(self$get_varNames())) {
          stop(paste0("Variety identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_varNames())) {
          stop(paste0("Variety identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_varNames())
        }
      }
      return(id)
    },

    #' @description Create a TPE analysis object
    #' @param name A character string of TPE analysis name
    createTPE = function(name = "TPEa_1") {
      self$TPEanalysis <- append(self$TPEanalysis,
                                 TPEa$new(name, self$model, self))
    },

    #' @description Create a TPE analysis object
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param name A character string identifier of the grid
    #' @param varID A variety identifier to use for simulation on the grid
    #' @param latres A numeric value of the latitude resolution of the grid
    #' @param lonres A numeric value of the longitude resolution of the grid
    #' @param cols A numeric value of the number of columns in the grid
    #' @param rows A numeric value of the number of rows in the grid
    #' @param lon A numeric value of the starting longitude of the grid in
    #' decimal degrees
    #' @param lat A numeric value of the starting latitude of the grid in
    #' decimal degrees
    #' @param multigrid A boolean indicating if one grid should be created
    #' for each genotype with the same cols, rows, lon and lat. If true, varID
    #' will be ignored
    createGrid = function(tpeID = 1, name="g1", varID=NA, latres=0.35,
                          lonres=0.5, cols=5, rows=5, lon=NA, lat=NA,
                          multigrid=F) {
      self$TPEanalysis[[tpeID]]$createGrid(name, varID, latres, lonres, cols,
                                           rows, lon, lat, multigrid)
    },

    #' @description Generate climate data for one or several grids
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
    #' @param rcp A character string with the name of the Representative
    #' Concentration Pathway to use. One of the following options
    #' c("rcp26","rcp45","rcp60","rcp85")
    #' @param year A numeric value of the year to simulate climate
    #' (this can include years from 2013 to 2099)
    #' @param yearNb A numeric value of the number of years to simulate
    #' @param modelNb A character string of the general circulation model
    #' identifier to use, see \code{generateClimate}
    #' @param path A character string with the path to the marksim standalone.
    #' For the moment, this path can not contain spaces
    #' @param pathCLI Optional. A character string with the path to CLI folder.
    #' If no value is provided, the CLI folder will be considered in the same
    #' folder as the marksim standalone. For the moment, this path can not
    #' contain spaces
    #' @param seed A numeric value of the seed to use to generate climate
    #' @param filesE Boolean. If weather files already exist
    #' @param verbose Boolean. If messages about starting climate generation
    #' should be shown
    genClimate = function(tpeID=1, gridID=NA, rcp="rcp26", year=2014, yearNb=1,
                          modelNb="00000000000000000", path=NA, pathCLI=NA,
                          seed=NA, filesE=F, verbose=T) {
      self$TPEanalysis[[tpeID]]$genClimate(gridID, rcp, year, yearNb, modelNb,
                                           path, pathCLI, seed, filesE, verbose)
    },

    #' @description Run simulation on one or several grids
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
    #' @param varID A variety identifier to use for
    #' simulation on the grid. If NA, will use the variety attached to grid
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. Vector of trait names to extract from
    #' simulations. This will delete the simulations and only keep the max for
    #' each year
    #' @param savePath Optional. A character string of the path where to save
    #' simulation files. If NULL (default), will not save simulations
    runGridSim = function(tpeID=1, gridID=NA, varID=NA, soilData=soil,
                          latlonData=lat_lon, cumulP=cumul,
                          traitList=NULL, savePath=NULL) {

      self$TPEanalysis[[tpeID]]$runGridSim(gridID, varID, soilData, latlonData,
                                           cumulP, traitList, savePath)
    },

    #' @description Create raster map
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param name A character string defining the name of the map
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @param res Not used at the moment. A numeric value in sec of the
    #' resolution of world map to use. Options are c(1, 150, 900)
    #' for respectively 30sec, 2.5min, 15min
    createMap = function(tpeID=1, name="map1", bounds=NA, res=150) {
      self$TPEanalysis[[tpeID]]$createMap(name, bounds, res)
    },

    #' @description Run TPE clustering by performing Principle Component
    #' Analysis (PCA) and Hierarchical Clustering on Principle Component (HCPC)
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param mapID A value or list of values of map identifiers
    #' (either index or names)on which to perform clustering
    #' @param traitList Vector of variable names to be used for PCA
    #' @param nbDim Integer of number of dimensions to use for PCA
    #' @param nbClust Integer of number of clusters to use for HCPC
    runClustering = function(tpeID=1, mapID=1, traitList=c("GrainYieldPop"),
                             nbDim=5, nbClust=3) {
      self$TPEanalysis[[tpeID]]$runClustering(mapID, traitList, nbDim, nbClust)
    },

    #' @description Create plot on map based on grid simulation
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param mapID Id of map to plot
    #' @import ggplot2
    plotMap = function(tpeID=1, mapID=1) {
      self$TPEanalysis[[tpeID]]$plotMap(mapID)
    },

    #' @description Run parameter estimation
    #' @import DEoptim
    #' @description Run parameter estimation
    #' @param varID A value or list of values of variety identifier
    #' (either index or name)
    #' @param estimID A value of estimation identifier (either index or name)
    #' @param maxiter A numeric value of the maximum number of iteration
    #' for DEoptim
    #' @param paramnames A vector of parameter names to be estimated
    #' @param metric A character string with the name of the metric to use
    #' for fitness computation. Options are c("RMSE","MAE","MSE")
    #' @param score_fn A function to compute fitness, see \code{get_score}
    #' @param weigh_fn Not used for the moment
    #' @param bounds A matrix with lower (row1) and upper (row2) bounds for
    #' each of the parameters in `paramnames` (cols)
    #' @param ... Additional parameters to be passed to DEoptim.control
    runEstimation = function(varID=1, estimID=1, maxiter=2000, paramnames=NA,
                             metric="RMSE", score_fn=get_score, weigh_fn=NA,
                             bounds=NA, ...) {
      args <- list(...)
      for(i in 1:length(varID)) {
        id <- self$get_varid(varID[i])
        self$varieties[[id]]$runEstimation(estimID, maxiter, paramnames, metric,
                                           score_fn, weigh_fn, bounds, args)
      }
    }

  ),

  private = list(
    gridnames = NULL,
    varnames = NULL,
    genotypes = NULL,
    mapnames = NULL
  )
)
