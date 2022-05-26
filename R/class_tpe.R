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
    #' @field name Identifier of the TPE analysis
    name = NULL,
    #' @field model Name of the crop model used for simulations
    model = NULL,
    #' @field varieties List of varieties names used for TPE analysis
    varieties = c(),
    #' @field environments List of environment names used for TPE analysis
    environments = c(),
    #' @field genotypes List of alternate varieties names used for TPE analysis
    genotypes = c(),
    #' @field grid Simulation grid
    grid = NULL,
    #' @field parameters A vector or dataframe with all parameters used for
    #' simulation with the corresponding `model`
    parameters = NULL,
    #' @field weathers A dataframe or list of dataframe with weather data for
    #' each of the `environments`
    weathers = NULL,
    #' @field observations A dataframe or list of dataframe with observation *
    #' data
    observations = NULL,
    #' @field estimParam Values of estimated parameters
    estimParam = NULL,

    #' @description Create a new TPE analysis object
    #' @param name Identifier of the TPE analysis
    #' @param model Name of the crop model used for simulations
    #' @param varieties List of varieties
    #' @param environments List of environments
    #' @param genotypes Optional. List of alternate variety names
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
          stop("Length of genotypes does not match length of varieties.
               Please either provide an alternate name for each variety or use
               genotypes=NA.")
        } else {
          self$genotypes <- genotypes
        }
      } else {
        self$genotypes <- self$varieties
      }
      self$parameters <- parameters
      self$initMessage()
    },

    #' @description Set crop model
    #' @param val New model name
    set_model = function(val) {
      self$model <- val
    },

    #' @description Set weather data
    #' @param val Dataframe (or list of df) of weather data
    set_weather = function(val) {
      self$weathers <- val
    },

    #' @description Set observation data
    #' @param val Dataframe (or list of df) of observation data
    set_obs = function(val) {
      self$observations <- val
    },

    #' @description Set estimated parameter values
    #' @param val Vector of parameter values
    set_eparam = function(val) {
      self$estimParam <- val
    },

    #' @description Update parameter with estimated parameter values
    #' @param val Vector of parameter values
    #' @param names Vector of parameter names
    update_param = function(val,names) {
      for(p in 1:length(names)) {
        self$parameters[,which(colnames(self$parameters) ==
                                 names[[p]])] <- val[[p]]
      }
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
    #' @param name Identifier of the grid
    #' @param res Resolution of the grid
    #' @param cols Number of columns in the grid
    #' @param rows Number of rows in the grid
    #' @param lon Optional. Starting longitude of the grid
    #' @param lat Optional. Starting latitude of the grid
    createGrid = function(name="grid1", res=5, cols=5, rows=5, lon=NA, lat=NA) {
      self$grid <- TPEgrid$new(name, res, cols, rows, lon, lat)
    },

    #' @description Generate climate data for each point of the grid
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    genClim = function(rcp="rcp26", year=2015, yearNb=99,
                       modelNb="00000000000000000", path=NA, pathCLI=NA) {
      for(i in 1:length(self$grid$gridPoints)) {
        for(j in 1:length(self$grid$gridPoints[[i]])) {
          self$grid$gridPoints[[i]][[j]]$genClimate(rcp, year, yearNb, modelNb,
                                                    path, pathCLI)
        }
      }
    },

    #' @description Generate climate data for each point of the grid
    #' @param row Row of parameter dataframe to use
    runGridSim = function(row=1) {
      param <- self$parameters[1,]
      for(i in 1:length(self$grid$gridPoints)) {
        for(j in 1:length(self$grid$gridPoints[[i]])) {
          self$grid$gridPoints[[i]][[j]]$runSimulation(param)
        }
      }
    },

    #' @description Run parameter estimation
    #' @param maxiter Maximum number of iteration for DEoptim
    #' @param paramnames Vector of parameter names to be estimated
    #' @param metric Metric to use for fitness computation
    #' @param score_fn Function to compute fitness, see \code{get_score}
    #' @param weigh_fn Not used
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
      self$set_eparam(as.vector(resEstim$optim$bestmem))
      self$update_param(self$estimParam, paramnames)
    }
  ),
  private = list()
)
