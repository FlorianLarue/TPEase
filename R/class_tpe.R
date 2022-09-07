#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' The TPE Analysis object "TPEa" is an environment containing all objects
#' used for a TPE analysis (varieties, environments, maps, etc.)
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEa <- R6::R6Class("TPEa",
  public = list(
    #' @field name A character string identifier of the TPE analysis
    name = NULL,
    #' @field model A character string with the name of the crop model
    #' used for simulations (only "Samara" is supported for now)
    model = NULL,
    #' @field varieties A list of varieties objects
    varieties = list(),
    #' @field environments A vector of environment names used for calibration
    environments = list(),
    #' @field grids A vector of simulation grids
    grids = list(),
    #' @field maps A list of raster maps for each of the `grids`
    maps = list(),
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE analysis object
    #' @param name A character string identifier of the TPE analysis
    #' @param model A character string with the name of the crop model used
    #' for simulations
    #' @param varieties A list of varieties names
    #' @param environments A list of environments names
    #' @param genotypes Optional. A list of alternate variety names
    #' @param vparameters A vector or dataframe with variety specific parameters
    #' @param eparameters A vector or dataframe with environment specific
    #' @param parameters Optional. A vector or dataframe with all crop model
    #' parameters (variety x environment)
    #' @param vep Optional. If `parameters`is provided this argument
    #' controls in what order parameters are given, either parameters for all
    #' varieties of environment 1, then all for environment 2, etc. (by default,
    #' TRUE) or parameters for all environments for variety 1, then for variety
    #' 2, etc. (FALSE)
    #' @return A new `TPEa` object.
    initialize = function(name="TPEa_1", model="Samara", varieties=NA,
                          environments=NA, genotypes=NA, vparameters=NA,
                          eparameters=NA, parameters=NA, vep=TRUE) {

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
          self$varieties <- append(self$varieties, TPEvar$new(
            name = as.character(private$varnames[i]),
            alt = as.character(private$genotypes[i]),
            parent = self))

          if(class(vparameters) == "data.frame" && nrow(vparameters) >= i) {
            self$varieties[[i]]$set_param(vparameters[i,])
          } else {
            if(vep) {
              if(class(parameters) == "data.frame" && nrow(parameters) >= i) {
                self$varieties[[i]]$set_param(parameters[i,])
              } else {
                vParamMissings <- c(vParamMissings, private$varnames[i])
              }
            } else if(length(environments) > 1 || !is.na(environments)) {
              nbEnv <- length(environments)
              if(class(parameters) == "data.frame" &&
                 nrow(parameters) >= nbEnv + i) {
                self$varieties[[i]]$set_param(parameters[nbEnv + i,])
              } else {
                vParamMissings <- c(vParamMissings, private$varnames[i])
              }
            } else {
              stop("Please provide at least one environment name.")
            }
          }
        }
      } else {
        stop("Please provide at least one variety name.")
      }

      eParamMissings <- c()
      if(length(environments) > 1 || !is.na(environments)) {
        private$envnames <- environments
        for(j in 1:length(environments)) {
          self$environments <- append(self$environments, TPEenv$new(
            name = as.character(private$envnames[j]),
            parent = self))

          if(class(eparameters) == "data.frame" && nrow(eparameters) >= j) {
            self$environments[[j]]$set_param(eparameters[j,])
          } else {
            if(vep) {
              nbV <- ((j-1)*length(varieties)) + 1
              if(class(parameters) == "data.frame" && nrow(parameters) >= nbV) {
                self$environments[[j]]$set_param(parameters[nbV,])
              } else {
                eParamMissings <- c(eParamMissings, private$envnames[j])
              }
            } else {
              if(class(parameters) == "data.frame" && nrow(parameters) >= j) {
                self$environments[[j]]$set_param(parameters[j,])
              } else {
                eParamMissings <- c(eParamMissings, private$envnames[j])
              }
            }
          }
        }
      } else {
        stop("Please provide at least one environment name.")
      }
      self$initMessage(vParamMissings, eParamMissings)
    },


    #' @description Confirm creation of TPE analysis object
    #' @param vParamMissings Name of varieties without parameters
    #' @param eParamMissings Name of environments without parameters
    initMessage = function(vParamMissings, eParamMissings) {
      if(length(vParamMissings) > 0) {
        warning(paste0("No parameters were provided for ",
                 ifelse(length(vParamMissings > 1), "varieties [", "variety ["),
                 paste0(vParamMissings, collapse=" "),
                 "]. Simulations will not be possible. ",
                 "You can set parameters by running set_param() on the ",
                 "variety object."), call.=F)
      }

      if(length(eParamMissings) > 0) {
        warning(paste0("No parameters were provided for ",
                 ifelse(length(eParamMissings > 1), "environments [",
                        "environments ["),
                 paste0(eParamMissings, collapse=" "),
                 "]. Simulations will be run with default parameters. ",
                 "You can set parameters by running set_param() on the ",
                 "environment object."), call.=F)
      }

      cat(paste0("TPE analysis ", self$name, " created containing ",
           length(private$varnames), ifelse(length(private$varnames) > 1,
                                            " varieties "," variety "), "and ",
           length(private$envnames), ifelse(length(private$envnames) > 1,
                                            " environments", " environment"),
           "\n"))
    },


    ## Setters

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

    #' @description Set environments, this is a list of TPEenv objects,
    #' use with caution this will overwrite existing environments
    #' @param val New environments
    set_env = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEenv") {
          self$environments <- list(val)
        } else {
          stop(paste0("Error. Trying to set a non environment object into ",
                      self$name))
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEenv"))) == 0) {
          self$environments <- val
        } else {
          stop(paste0("Error. Trying to set a non environment object into ",
                      self$name))
        }
      } else {
        stop("Depth of environments can not exceed 1")
      }
    },

    #' @description Set grids, this is a list of grid objects,
    #' use with caution this will overwrite existing grids
    #' @param val New grids
    set_grid = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEgrid") {
          self$grids <- list(val)
        } else {
          stop(paste0("Error. Trying to set a non grid object into ",
                      self$name))
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEgrid"))) == 0) {
          self$grids <- val
        } else {
          stop(paste0("Error. Trying to set a non grid object into ",
                      self$name))
        }
      } else {
        stop("Depth of grids can not exceed 1")
      }
    },

    #' @description Set maps, this is a list of map objects,
    #' use with caution, this will overwrite existing maps
    #' @param val New maps
    set_map = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEmap") {
          self$maps <- list(val)
        } else {
          stop(paste0("Error. Trying to set a non map object into ",
                      self$name))
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEmap"))) == 0) {
          self$maps <- val
        } else {
          stop(paste0("Error. Trying to set a non map object into ",
                      self$name))
        }
      } else {
        stop("Depth of maps can not exceed 1")
      }
    },

    ## Setters of sub-objects

    #' @description Set observation data
    #' @param varID A value of variety identifier (either index or name)
    #' @param val A dataframe or list of dataframes with observations for each
    #' of the `environments` (and eventually for each replicate)
    set_obs = function(varID=1, val) {
      id <- self$get_varid(varID)
      self$varieties[[id]]$set_obs(val)
    },

    #' @description Set weather data
    #' @param envID A value of environment identifier (either index or name)
    #' @param val New weather
    set_weather = function(envID=1, val) {
      id <- self$get_envid(envID)
      self$environments[[id]]$set_weather(val)
    },

    ## Getters

    #' @description Get names of all grids
    get_gridNames = function() {
      return(private$gridnames)
    },

    #' @description Get names of all varieties
    get_varNames = function() {
      return(private$varnames)
    },

    #' @description Get names of all genotypes
    get_genotypes = function() {
      return(private$genotypes)
    },

    #' @description Get names of all environments
    get_envNames = function() {
      return(private$envnames)
    },

    #' @description Get names of all maps
    get_mapNames = function() {
      return(private$mapnames)
    },

    ## Getters of sub-objects

    #' @description Get grid id
    #' @param val Either grid name or a grid id (will return the grid id)
    get_gridid = function(val) {
      if(class(val) == "numeric") {
        if(val > length(self$get_gridNames())) {
          stop(paste0("Grid identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_gridNames())) {
          stop(paste0("Grid identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_gridNames())
        }
      }
      return(id)
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

    #' @description Get env id
    #' @param val Either env name or a env id (will return the env id)
    get_envid = function(val) {
      if(class(val) == "numeric") {
        if(val > length(self$get_envNames())) {
          stop(paste0("Environment identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_envNames())) {
          stop(paste0("Environment identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_envNames())
        }
      }
      return(id)
    },

    #' @description Get map id
    #' @param val Either map name or a map id (will return the map id)
    get_mapid = function(val) {
      if(class(val) == "numeric") {
        if(val > length(self$get_mapNames())) {
          stop(paste0("Map identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_mapNames())) {
          stop(paste0("Map identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_mapNames())
        }
      }
      return(id)
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
    #' @param multigrid A boolean indicating if one grid should be created
    #' for each genotype with the same cols, rows, lon and lat.
    createGrid = function(name="g1", res=0.5, cols=5, rows=5, lon=NA, lat=NA,
                          multigrid=F) {
      if(name %in% self$get_gridNames()) {
        stop(paste("Grid with name", name,"already exists.",
                     "Please provide a unique identifier."))
      }

      if(!multigrid) {
        self$grids <- append(self$grids, TPEgrid$new(name, res, cols, rows, lon,
                                                     lat, self, NA))
        private$gridnames <- c(private$gridnames, name)
      } else {
        for(i in 1:length(private$varnames)) {
          gname <- paste0(name, "_", private$varnames[i])
          self$grids <- append(self$grids, TPEgrid$new(gname, res, cols, rows,
                                                       lon, lat, self,
                                                       self$varieties[[i]]))
          private$gridnames <- c(private$gridnames, gname)
        }
      }
    },

    #' @description Generate climate data for one or several grids
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
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
    #' @param filesE Boolean. If weather files already exist
    #' @param verbose Boolean. If messages about starting climate generation
    #' should be shown
    genClimate = function(gridID=NA, rcp="rcp26", year=2014, yearNb=1,
                          modelNb="00000000000000000", path=NA, pathCLI=NA,
                          filesE=F, verbose=F) {
      if(sum(!is.na(gridID)) == 0) {
        idg <- self$get_gridNames()
      } else {
        idg <- gridID[!is.na(gridID)]
      }

      for(i in 1:length(idg)) {
        id <- self$get_gridid(idg[i])
        if(verbose) {
          cat(paste("Generating climate for grid", self$get_gridNames()[id],
                    "this may take some time \n"))
        }
        self$grids[[id]]$genClimate(rcp, year, yearNb, modelNb, path,
                                    pathCLI, filesE, verbose)
      }
    },

    #' @description Run simulation on one or several grids
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
    #' @param varID A variety identifier to use for
    #' simulation on the grid. If NA, will use the variety attached to grid
    #' @param trait A character string with the trait name for grid res matrix
    #' @param year A numeric value with the year to run the simulation
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    runGridSim = function(gridID=NA, varID=NA, trait="GrainYieldPopFin",
                          year=2015, soilData=soil, latlonData=lat_lon) {
      if(sum(!is.na(gridID)) == 0) {
        idg <- self$get_gridNames()
      } else {
        idg <- gridID[!is.na(gridID)]
      }

      for(i in 1:length(idg)) {
        id <- self$get_gridid(idg[i])
        if(!is.na(varID)) {
          idv <- self$get_varid(varID)
          self$grids[[id]]$set_var(self$varieties[[idv]])
        }
        self$grids[[id]]$runGridSim(trait, year, soilData, latlonData)
      }
    },

    #' @description Create raster map
    #' @param name A character string defining the name of the map
    #' @param varID A variety identifier to use on the map
    #' @param gridID A grid identifier to use on the map
    #' @param res A numeric value in sec of the resolution of world map to use
    #' Options are c(1, 150, 900) for respectively 30sec, 2.5min, 15min
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    createMap = function(name="map1", gridID=NA, varID=NA, res=150, bounds=NA) {
      if(name %in% self$get_mapNames()) {
        stop(paste("Map with name ", name, " already exist.",
                   "Please provide a unique name for each map"))
      } else {
        private$mapnames <- c(private$mapnames, name)
        self$maps <- append(self$maps, TPEmap$new(name, gridID, varID, res,
                                                  bounds, self))
      }
    },

    #' @description Create plot on map
    #' @param mapID A map identifier to plot
    #' (either index or name). By default will run on all maps
    plotMap = function(mapID=1) {
      if(sum(!is.na(mapID)) == 0) {
        idg <- self$get_mapNames()
      } else {
        idg <- mapID[!is.na(mapID)]
      }
      for(i in 1:length(idg)) {
        id <- self$get_mapid(idg[i])
        self$maps[[id]]$plotMap()
      }
    },

    #' @description Run parameter estimation
    #' @param varID A value or list of values of variety identifier
    #' (either index or name)
    #' @param envID A value or list of values of environment identifier
    #' (either index or name)
    #' @param maxiter A numeric value of the maximum number of iteration
    #' for DEoptim
    #' @param paramnames A vector of parameter names to be estimated
    #' @param metric A character string with the name of the metric to use
    #' for fitness computation. Options are c("RMSE","MAE","MSE")
    #' @param score_fn A function to compute fitness, see \code{get_score}
    #' @param weigh_fn Not used for the moment
    #' @param bounds A matrix with lower (col1) and upper (col2) bounds for
    #' each of the parameters in `paramnames` (rows)
    #' @import rsamara
    #' @import DEoptim
    runEstimation = function(varID=1, envID=1, maxiter=2000,paramnames=NA,
                             metric="RMSE", score_fn=get_score, weigh_fn=NA,
                             bounds=NA) {
      for(i in 1:length(varID)) {
        id <- self$get_varid(varID[i])
        weathers <- list()
        for(j in 1:length(envID)) {
          ide <- self$get_envid(envID[j])
          weathers <- append(weathers, list(self$environments[[ide]]$weather))
        }
        self$test <- weathers
        self$varieties[[id]]$runEstimation(maxiter, paramnames, metric,
                                           score_fn, weigh_fn, bounds, weathers,
                                           id)
      }
    }
  ),

  private = list(
    gridnames = NULL,
    varnames = NULL,
    genotypes = NULL,
    envnames = NULL,
    mapnames = NULL
  )
)
