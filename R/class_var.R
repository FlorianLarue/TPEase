#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEvar <- R6::R6Class("TPEvar",
  public = list(
    #' @field name A character string identifier of the variety
    name = NULL,
    #' @field alt A character string alternate adientifier of the variety
    alt = NULL,
    #' @field parent TPE analysis parent
    parent = NULL,
    #' @field parameters A dataframe with all parameters used for simulation
    parameters = NULL,
    #' @field estimParam A vector containing the values of estimated parameters
    estimParam = NULL,
    #' @field observations A dataframe or list of dataframe with observations
    #' for each of the `environments`. If several replicates, a list of lists
    #' (environment x replicate)
    observations = NULL,

    #' @description Create a new variety object
    #' @param name A character string identifier of the variety
    #' @param alt A character string alternate identifier of the variety
    #' @param parent TPE analysis parent
    #' @return A new `TPEvar` object.
    initialize = function(name="v1", alt="G1", parent) {
      self$name <- name
      self$alt <- alt
      self$parent <- parent
    },

    #' @description Set observation data
    #' @param val A dataframe or list of dataframes with observations for each
    #' of the `environments` (and eventually for each replicate)
    set_obs = function(val) {
      self$observations <- val
    },

    #' @description Set parameters
    #' @param val A dataframe with all crop model parameters
    set_param = function(val) {
      self$parameters <- val
    },

    #' @description Set estimated parameters
    #' @param val A vector with estimated parameter values
    #' @param names A vector with estimated parameter names
    set_eparam = function(val, names) {
      self$estimParam <- val
      self$update_param(self$estimParam, names)
    },

    #' @description Set estimated parameters
    #' @param val A vector with parameter values
    #' @param names A vector with estimated parameter names
    update_param = function(val,names) {
      for(p in 1:length(names)) {
        self$parameters[,which(colnames(self$parameters) ==
                                 names[[p]])] <- val[[p]]
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
    #' @param bounds A matrix with lower (col1) and upper (col2) bounds for
    #' @param weathers A list of weather data for each environment
    #' @param varID A numeric value of the index of variety currently estimating
    #' @import rsamara
    #' @import DEoptim
    runEstimation = function(maxiter, paramnames, metric, score_fn, weigh_fn,
                             bounds, weathers, varID) {
      if(!is.null(self$parameters) & !is.null(self$observations)) {
        if((length(weathers) > 1 & depth(self$observations) < 3) |
           length(self$observations) < length(weathers)) {
          stop(paste("Could not launch parameter estimation",
                     "number of observations did not match the number",
                     "of environments. Please provide a list",
                     "(one element per environment) of lists",
                     "(one element per replicate) or select less environments",
                     "and retry to run estimation."))
        } else {
          DEParams <- DEoptim.control(itermax=maxiter,strategy=2,trace=1,
                                      NP=10*length(paramnames))

          resEstim <- DEoptim::DEoptim(estim_param, paramBounds[,1],
                                       paramBounds[,2], control=DEParams,
                                       self$parameters, paramnames, weathers,
                                       self$observations, score_fn, metric,
                                       weigh_fn, varID, fnMap=NULL)
        }
      } else {
        warning(paste("Missing parameters and/or observations for variety",
                      self$name), call.=F)
      }
    }
  )
)
