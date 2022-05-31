#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
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

    #' @description Create a new variety object
    #' @param name A character string identifier of the variety
    #' @param alt A character string alternate identifier of the variety
    #' @param parent TPE analysis parent
    #' @return A new `TPEa` object.
    initialize = function(name="v1", alt="G1", parent) {
      self$name <- name
      self$alt <- alt
      self$parent <- parent
    },

    #' @description Set parameters
    #' @param val A dataframe with all crop model parameters
    set_param = function(val) {
      self$parameters <- val
    },

    #' @description Set estimated parameters
    #' @param val A vector with estimated parameter values
    set_eparam = function(val) {
      self$estimParam <- val
    },

    #' @description Set estimated parameters
    #' @param val A vector with parameter values
    #' @param names A vector with estimated parameter names
    update_param = function(val,names) {
      for(p in 1:length(names)) {
        self$parameters[,which(colnames(self$parameters) ==
                                 names[[p]])] <- val[[p]]
      }
    }
  )
)
