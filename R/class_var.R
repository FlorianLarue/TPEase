#' R6 Class representing a TPEase Variety
#'
#' @description
#' Variety object containing all information about a crop variety
#'
#' @usage
#' Varieties are automatically created when the parent \code{TPEase} is
#' initialized
#'
#' @details
#' The \code{TPEaseVar} object is the first brick built upon the \code{TPEase}.
#' It contains basic information about a crop variety as well as a list of
#' \code{VarEstim} objects (parameter estimation)
#'
#' @import R6
#' @export
TPEaseVar <- R6::R6Class("TPEaseVar",
  public = list(
    #' @field name The \code{TPEaseVar} identifier
    name = NULL,
    #' @field alt An alternate name for the variety
    alt = NULL,
    #' @field parent The \code{TPEase} object parent of the \code{TPEaseVar}
    parent = NULL,
    #' @field parameters A \code{data.frame} with all parameters used
    #' for simulation
    parameters = NULL,
    #' @field estimations A list of \code{VarEstim} objects
    estimations = list(),
    #' @field test Debug
    test = NA,

    #' @description Create a new \code{TPEaseVar} object
    #' @param name A character string identifier of the TPEaseVar
    #' @param alt A character string alternate identifier of the variety
    #' @param parameters A dataframe with variety parameters
    #' @param eName A character string identifier of initial VarEstim object
    #' @param environments A list of environments names
    #' @param eparam A dataframe with environment parameters
    #' @param weathers A list of dataframes of weathers
    #' (each entry is for one environment)
    #' @param observations A list of dataframes of observations
    #' (each entry is for one environment)
    #' @param parent A TPEase object parent of the TPEaseVar
    #' @return A new `TPEaseVar` object.
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

    #' @description Get VarEstim names
    get_estimNames = function() {
      return(private$estimnames)
    },

    #' @description Get VarEstim id
    #' @param val A VarEstim identifier (either index or name)
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


    #' @description Set TPEaseVar parameters
    #' @param val A dataframe with variety parameters
    set_param = function(val) {
      self$parameters <- val
    },

    #' @description Set TPEaseVar estimated parameters
    #' @param val A vector with parameter values
    #' @param names A vector with estimated parameter names
    updateParam = function(val,names) {
      for(p in 1:length(names)) {
        self$parameters[,which(colnames(self$parameters) ==
                                 names[[p]])] <- val[[p]]
      }
    },

    #' @description Create a VarEstim object
    #' @param name A character string identifier of the VarEstim
    #' @param parent A TPEaseVar object parent of the VarEstim
    #' @param environments A vector of environments identifiers
    #' @param eparam A dataframe with parameters of environments
    #' @param weathers A dataframe with weather data of environments
    #' @param observations A dataframe with observations of environments for
    #' the variety TPEaseVar
    createEstim = function(name, parent, environments, eparam, weathers,
                           observations) {
      self$estimations <- append(self$estimations,
                                 VarEstim$new(name, parent, environments,
                                              eparam, weathers, observations))
      private$estimnames <- c(private$estimnames, name)
    },

    #' @description Run parameter estimation
    #' @param estimID A value of VarEstim identifier (either index or name)
    #' @param maxiter A numeric value of the maximum number of iteration
    #' for DEoptim
    #' @param paramnames A vector of parameter names to be estimated
    #' @param metric A character string with the name of the metric to use
    #' for fitness computation. Options are c("RMSE","MAE","MSE")
    #' @param score_fn A function to compute fitness, see \code{get_score}
    #' @param weigh_fn Not used for the moment
    #' TODO: implement weigh_fn
    #' @param bounds A matrix with lower (col1) and upper (col2) bounds for
    #' each of the parameters in `paramnames` (rows)
    #' @param args Additional parameters to be passed to DEoptim.control
    #' @import DEoptim
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
