#'#' R6 Class Representing an environment
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
TPEenv <- R6::R6Class("TPEenv",
  public = list(
    #' @field name A character string identifier of the variety
    name = NULL,
    #' @field parent TPE analysis parent
    parent = NULL,
    #' @field weather A dataframe with all weather data used for simulation
    weather = NULL,
    #' @field parameters A dataframe with all parameters used for simulation
    parameters = NULL,

    #' @description Create a new variety object
    #' @param name A character string identifier of the variety
    #' @param parent TPE analysis parent
    #' @return A new `TPEenv` object.
    initialize = function(name="e1", parent) {
      self$name <- name
      self$parent <- parent
    },

    #' @description Set weather
    #' @param val A dataframe with weather data
    set_weather = function(val) {
      self$weather <- val
    },

    #' @description Set parameters
    #' @param val A dataframe with parameter values
    set_param = function(val) {
      self$parameters <- val
    }
  )
)
