#' R6 Class representing an EstimEnv Environment
#'
#' @description
#' Environment object of a \code{VarEstim} containing all information about a
#' given environment used in the parameter estimation process
#'
#' @usage
#' \code{EstimEnv} are automatically created when initiating a \code{VarEstim}
#'
#' @details
#' The \code{EstimEnv} object is a subclass of \code{TPEaseEnv} specific for an
#' estimation process and containing all information of the Environment,
#' defined as a combination of : soil, weather and crop management data
#'
#' @import R6
#' @export
EstimEnv <- R6::R6Class("EstimEnv",
  inherit = TPEaseEnv,
  public = list(
    #' @field observations A \code{data.frame} of observations of a
    #' \code{TPEaseVar}
    observations = NULL,

    #' @description Create a new \code{EstimEnv} object
    #' @param name A \code{character} string identifier of the \code{EstimEnv}
    #' @param parent A \code{VarEstim} parent object of the \code{EstimEnv}
    #' @param parameters A \code{data.frame} with all crop model environment
    #' parameters
    #' @param weather A \code{data.frame} with weather data
    #' @param observations A \code{list} of \code{data.frames} of observations
    #' @return A new \code{EstimEnv} object
    initialize = function(name="e1", parent, parameters, weather,
                          observations) {
      super$initialize(name, parent, parameters, weather)
      self$observations <- observations
      self$soil$set_soilParam(colnames(self$soil$parameters),
                              self$parameters[colnames(self$soil$parameters)])
      self$cm$set_cmParam(colnames(self$cm$parameters),
                              self$parameters[colnames(self$cm$parameters)])
    }
  )
)
