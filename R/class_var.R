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
    #' @field parent analysis parent
    parent = NULL,
    #' @field parameters A dataframe with all parameters used for simulation
    parameters = NULL,
    #' @field estimations A list of estimation objects
    estimations = list(),

    #' @description Create a new variety object
    #' @param name A character string identifier of the variety
    #' @param alt A character string alternate identifier of the variety
    #' @param parameters A dataframe with variety parameters
    #' @param eName Name of initial estimation object
    #' @param environments List of environment names
    #' @param eparam Environment parameters
    #' @param weathers List of dataframes of weathers
    #' (each entry is for one environment)
    #' @param observations List of dataframes of observations
    #' (each entry is for one environment)
    #' @param parent TPE analysis parent
    #' @return A new `TPEvar` object.
    initialize = function(name="v1", alt="G1", parameters=NA, eName=NA,
                          environments=NA, eparam=NA, weathers=NA,
                          observations=NA, parent=NA) {
      self$name <- name
      self$alt <- alt
      self$parameters <- parameters
      self$parent <- parent
      if(!is.na(eName)) {
        self$createEstim(eName, self, environments, eparam, weathers,
                         observations)
      }
    },

    #' @description Get names of all estimations
    get_estimNames = function() {
      return(private$estimnames)
    },

    #' @description Get estimation id
    #' @param val Either estim name or a estim id (will return the estim id)
    get_estimid = function(val) {
      if(class(val) == "numeric") {
        if(val > length(self$get_estimNames())) {
          stop(paste0("Estimation identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_estimNames())) {
          stop(paste0("Variety identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_estimNames())
        }
      }
      return(id)
    },


    #' @description Set parameters
    #' @param val A dataframe with all crop model parameters
    set_param = function(val) {
      self$parameters <- val
    },

    #' @description Set estimated parameters
    #' @param val A vector with parameter values
    #' @param names A vector with estimated parameter names
    updateParam = function(val,names) {
      for(p in 1:length(names)) {
        self$parameters[,which(colnames(self$parameters) ==
                                 names[[p]])] <- val[[p]]
      }
    },

    #' @description Create estimation
    #' @param name Name of estimation
    #' @param parent Parent variety
    #' @param environments Name of environments
    #' @param eparam Parameters of environments
    #' @param weathers Weathers of environments
    #' @param observations Observations of environments
    createEstim = function(name, parent, environments, eparam, weathers,
                           observations) {
      self$estimations <- append(self$estimations,
                                 estimP$new(name, parent, environments, eparam,
                                            weathers, observations))
      private$estimnames <- c(private$estimnames, name)
    },

    #' @description Run parameter estimation
    #' @import DEoptim
    #' @description Run parameter estimation
    #' @param estimID A value of estimation identifier (either index or name)
    #' @param maxiter A numeric value of the maximum number of iteration
    #' for DEoptim
    #' @param paramnames A vector of parameter names to be estimated
    #' @param metric A character string with the name of the metric to use
    #' for fitness computation. Options are c("RMSE","MAE","MSE")
    #' @param score_fn A function to compute fitness, see \code{get_score}
    #' @param weigh_fn Not used for the moment
    #' @param bounds A matrix with lower (col1) and upper (col2) bounds for
    #' each of the parameters in `paramnames` (rows)
    #' @param args Additional parameters to be passed to DEoptim.control
    runEstimation = function(estimID=1, maxiter=2000, paramnames=NA,
                             metric="RMSE", score_fn=get_score, weigh_fn=NA,
                             bounds=NA, args) {
      id <- self$get_estimid(estimID)
      self$estimations[[id]]$runEstimation(maxiter, paramnames, metric,
                                           score_fn, weigh_fn, bounds, args)
    }
  ),
  private = list(
    estimnames = NULL
  )
)
