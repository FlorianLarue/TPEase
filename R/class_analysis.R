#' R6 Class representing a TPE analysis and simulation environment
#'
#' @description
#' Main object containing all objects and functions used for parameter
#' estimation and TPE analysis
#'
#' @usage
#' res <- TPEase$new("TPEase1", model, varieties, genotypes, vparameters,
#'                   environments, eparameters, eName, weathers, observations)
#'
#' @details
#' The \code{TPEase} object is the base building block of a
#' `TPE analysis and simulation environment` process, on which to built
#' additional bricks: crop model parameter estimation, statistical analysis
#' of simulation results, weather data generation, mapping and plotting, etc.
#' The \code{TPEase} can contain a list of \code{TPEaseVar} objects
#' (crop varieties) and a list of \code{TPEaseTPE} objects (TPE analyses)
#'
#' @import R6
#' @export
TPEase <- R6::R6Class("TPEase",
  public = list(
    #' @field name The \code{TPEase} identifier
    name = NULL,
    #' @field model The name of the crop model used in this \code{TPEase}
    #' (only "Samara" is supported at the momemnt)
    model = NULL,
    #' @field varieties A list of \code{TPEaseVar} objects
    varieties = list(),
    #' @field TPEanalyses A list of \code{TPEaseTPE} objects
    TPEanalyses = list(),

    #' @description Create a new TPEase object
    #' @param name A \code{character} string identifier of the TPEase
    #' @param model A \code{character} string identifier of the crop model used
    #' @param varieties A \code{vector} of varieties names
    #' @param genotypes Optional. A \code{vector} of alternate varieties names
    #' @param vparameters A \code{vector} or \code{data.frame}
    #' with variety specific parameters
    #' @param environments A \code{vector} of environments names
    #' @param eparameters A \code{vector} or \code{data.frame}
    #' with environment specific parameters
    #' @param eName A \code{character} string identifier of the initial
    #' \code{EstimVar} object
    #' @param weathers A \code{list} (each entry is an environment)
    #' of \code{data.frames} containing weather data
    #' @param observations A \code{list} (each entry is a variety)
    #' of \code{lists} (each entry is an environment) of \code{data.frames}
    #' containing observations (of a given \code{TPEaseVar} [first key] in a
    #' given \code{TPEaseEnv} [second key])
    #' @return A new `TPEase` object
    initialize = function(name="TPEase_1", model="Samara", varieties=NA,
                          genotypes=NA, vparameters=NA, environments=NA,
                          eparameters=NA, eName="estim1", weathers=NA,
                          observations=NA) {
      self$name <- as.character(name)
      self$model <- as.character(model)
      vParamMissings <- c()
      if(length(varieties) > 1 || !is.na(varieties)) {
        private$varnames <- as.character(varieties)
        if(length(genotypes) != length(varieties)) {
          private$genotypes <- as.character(varieties)
          warning(paste("Length of genotypes does not match",
                        "length of varieties, name of varieties will be used."),
                  call.=F)
        } else {
          private$genotypes <- genotypes
        }
        for(i in 1:length(varieties)) {
          if(class(vparameters) == "data.frame" && nrow(vparameters) >= i) {
            param <- vparameters[i,]
            rownames(param) <- NULL
          } else {
            vParamMissings <- c(vParamMissings, private$varnames[i])
            param <- NULL
          }
          self$varieties <- append(self$varieties, TPEaseVar$new(
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

      private$initMessage(vParamMissings)
    },

    #' @description Set TPEase name
    #' @param val New TPEase name
    set_name = function(val) {
      self$name <- as.character(val)
    },

    #' @description Set model name
    #' @param val New model name
    set_model = function(val) {
      self$model <- as.character(val)
    },

    #' @description Set varieties
    #' @param val New \code{TPEaseVar} objects
    set_var = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEaseVar") {
          self$varieties <- list(val)
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEaseVar"))) == 0) {
          self$varieties <- val
        } else {
          stop(paste0("Error. Trying to set a non variety object into ",
                      self$name))
        }
      } else {
        stop("Depth of varieties can not exceed 1")
      }
    },

    #' @description Get names of all TPEaseVar
    get_varNames = function() {
      return(private$varnames)
    },

    #' @description Get names of all genotypes
    get_genotypes = function() {
      return(private$genotypes)
    },

    #' @description Get TPEaseVar id
    #' @param val A variety identifier
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

    #' @description Create a \code{TPEaseTPE} object
    #' @param name A character string of TPE name
    createTPE = function(name = "TPEa_1") {
      self$TPEanalyses <- append(self$TPEanalyses,
                                 TPEaseTPE$new(name, self$model, self))
    },

    #' @description Create a \code{TPEGrid} object
    #' @param tpeID A \code{TPEaseTPE} identifier on which to create grid
    #' @param name A \code{character} string identifier of the grid
    #' @param varID A \code{TPEaseVar} identifier to use for simulation
    #' on the grid
    #' @param latres A \code{numeric} value of the latitude resolution
    #' of the grid
    #' @param lonres A \code{numeric} value of the longitude resolution
    #' of the grid
    #' @param cols A \code{numeric} value of the number of columns in the grid
    #' @param rows A \code{numeric} value of the number of rows in the grid
    #' @param lon A \code{numeric} value of the starting longitude
    #' of the grid in decimal degrees
    #' @param lat A \code{numeric} value of the starting latitude
    #' of the grid in decimal degrees
    #' @param multigrid A \code{boolean }indicating if one grid should be
    #' created for each genotype with the same \code{cols}, \code{rows},
    #' \code{lon} and \code{lat}. If true, \code{varID} will be ignored
    createGrid = function(tpeID = 1, name="g1", varID=NA, latres=0.35,
                          lonres=0.5, cols=5, rows=5, lon=NA, lat=NA,
                          multigrid=F) {
      self$TPEanalyses[[tpeID]]$createGrid(name, varID, latres, lonres, cols,
                                           rows, lon, lat, multigrid)
    },

    #' @description Generate climate data for one or several \code{TPEGrid}
    #' @param tpeID A \code{TPEaseTPE} identifier of the grid's parent
    #' @param gridID Optional. A \code{vector} of \code{TPEGrid} identifiers
    #' (either index or name). By default will run all grids of the \code{tpeID}
    #' @param rcp A \code{character} string of the name of the Representative
    #' Concentration Pathway to use. One of the following options :
    #' \code{"rcp26","rcp45","rcp60","rcp85"}
    #' @param year A \code{numeric} value of the year to simulate climate
    #' (this can include years from 2013 to 2099)
    #' @param yearNb A \code{numeric} value of the number of
    #' replicates to simulate
    #' @param modelNb A \code{character} string of the general circulation model
    #' identifier to use, see \code{generateClimate}
    #' @param path A \code{character} string of the path to the
    #' marksim standalone (careful, this path can not contain any spaces)
    #' @param pathCLI Optional. A \code{character} string of the path to
    #' CLI folder. If no value is provided, the CLI folder will be considered
    #' in the same folder as the marksim standalone
    #' @param seed A \code{numeric} value of the seed to use to generate climate
    #' @param filesE A \code{boolean} indicating ff weather files already exist
    #' and should be used (existing weather files should be located in a
    #' "weathers" subfolder of the \code{path})
    #' @param verbose A \code{boolean} indicating ff messages about starting
    #' climate generation should be shown
    genClimate = function(tpeID=1, gridID=NA, rcp="rcp26", year=2014, yearNb=1,
                          modelNb="00000000000000000", path=NA, pathCLI=NA,
                          seed=NA, filesE=F, verbose=T) {
      self$TPEanalyses[[tpeID]]$genClimate(gridID, rcp, year, yearNb, modelNb,
                                           path, pathCLI, filesE, verbose, seed)
    },

    #' @description Run simulation on one or several \code{TPEGrid}
    #' @param tpeID A \code{TPEaseTPE} identifier on which to run
    #' grid simulations
    #' @param gridID Optional. A \code{vector} of \code{TPEGrid} identifiers
    #' (either index or name). By default will run all grids
    #' @param varID A \code{TPEaseVar} identifier to use for simulation
    #' on the grid. If NA, will use the variety attached to grid
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' TODO: implement generic methods for soil, latlon, cumul
    #' @param traitList Optional. A \code{vector} of trait names to extract from
    #' simulations. This will delete the simulations and only keep the
    #' maximum values for each year of the selected traits
    #' @param savePath Optional. A \code{character} string of the path where to
    #' save simulation files. If NULL (default), will not save simulations
    runGridSim = function(tpeID=1, gridID=NA, varID=NA, soilData=soil,
                          latlonData=lat_lon, cumulP=cumul,
                          traitList=NULL, savePath=NULL) {

      self$TPEanalyses[[tpeID]]$runGridSim(gridID, varID, soilData, latlonData,
                                           cumulP, traitList, savePath)
    },

    #' @description Create \code{TPEMap} object
    #' @param tpeID A \code{TPEaseTPE} on which to create the map
    #' @param name A \code{character} string identifier of the map
    #' @param bounds Optional. A \code{vector} of four \code{numeric} values
    #' as decimal degree of north, east, south, west bounds to crop
    #' the world map
    createMap = function(tpeID=1, name="map1", bounds=NA) {
      self$TPEanalyses[[tpeID]]$createMap(name, bounds)
    },

    #' @description Run TPE clustering by performing Principle Component
    #' Analysis (PCA) and Hierarchical Clustering on Principle Component (HCPC)
    #' @param tpeID A \code{PEaseTPE} identifier parent of the \code{TPEMap}
    #' @param mapID A value or list of values of \code{TPEMap} identifiers
    #' (either index or names) on which to perform clustering
    #' @param traitList A \code{vector} of trait names to be used for PCA
    #' @param nbDim An \code{integer} of number of dimensions to use for PCA
    #' @param nbClust An \code{integer} of number of clusters to use for HCPC
    runClustering = function(tpeID=1, mapID=1, traitList=c("GrainYieldPop"),
                             nbDim=5, nbClust=3) {
      self$TPEanalyses[[tpeID]]$runClustering(mapID, traitList, nbDim, nbClust)
    },

    #' @description Create plot on map based on grid simulation
    #' @param tpeID A \code{TPEeaseTPE} identifier parent of the \code{TPEMap}
    #' @param mapID A \code{TPEMap} identifier to plot
    #' @param trait A \code{character} string identifier of the data to plot,
    #' by default will plot the cluster computed by the runClustering function
    #' of the TPE analysis object
    #' @param isFactor A \code{boolean} indicating if the trait should be
    #' considered as a factor for plotting (cluster is a factor)
    #' @import ggplot2
    plotMap = function(tpeID=1, mapID=1, trait="cluster", isFactor=T) {
      self$TPEanalyses[[tpeID]]$plotMap(mapID, trait, isFactor)
    },

    #' @description Add ggplot2 objects to existing plots
    #' @param tpeID A \code{numeric} value identifier of the \code{TPEaseTPE}
    #' @param mapID A \code{TPEMap} identifier containing the plot
    #' @param plotID A \code{numeric} value identifier of the plot
    #' @param plotAdd A list of \code{ggplot2} objects to pass to the plot
    #' @import ggplot2
    addToPlot = function(tpeID=1, mapID=1, plotID=1, plotAdd) {
      self$TPEanalyses[[tpeID]]$addToPlot(mapID, plotID, plotAdd)
    },

    #' @description Print a given map
    #' @param tpeID A \code{numeric} value identifier of the \code{TPEaseTPE}
    #' @param mapID A \code{numeric} value identifier of the \code{TPEMap}
    #' @import ggplot2
    print_maps = function(tpeID=1, mapID=1) {
      plotList <- self$TPEanalyses[[tpeID]]$maps[[mapID]]$plots
      for(i in 1:length(plotList)) {
        print(plotList[[i]])
      }
    },

    #' @description Run parameter estimation for a given variety
    #' @param varID A value or list of values of \code{TPEaseVar} identifier(s)
    #' (either index or name)
    #' @param estimID A \code{VarEstim} identifier (either index or name)
    #' @param maxiter A \code{numeric} value of the maximum number of iteration
    #' for DEoptim
    #' @param paramnames A \code{vector} of parameter names to be estimated
    #' @param metric A \code{character} string with the name of the metric
    #' to use for fitness computation. Options are c("RMSE","MAE","MSE")
    #' @param score_fn A \code{function} to compute fitness,
    #' see \code{get_score}
    #' @param weigh_fn Not used for the moment
    #' TODO: implement weigh_fn
    #' @param bounds A \code{matrix} or \code{data.frame} with lower (row1)
    #' and upper (row2) bounds for each of the parameters in `paramnames` (cols)
    #' @param ... Additional parameters to be passed to \code{DEoptim.control}
    #' @import DEoptim
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
    mapnames = NULL,

    initMessage = function(vParamMissings) {
      if(length(vParamMissings) > 0) {
        warning(paste0("No parameters were provided for ",
                       ifelse(length(vParamMissings > 1),
                              "varieties [", "variety ["),
                       paste0(vParamMissings, collapse=" "),
                       "]. Simulations will not be possible. ",
                       "You can set parameters by running set_param() on the ",
                       "variety object."), call.=F)
      }

      cat(paste0("TPEase analysis ", self$name, " created containing ",
                 length(private$varnames), ifelse(length(private$varnames) > 1,
                 " varieties "," variety "), "\n"))
    }
  )
)
