#'#' R6 Class Representing an environment
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEenv <- R6::R6Class("TPEenv",
  public = list(
    #' @field name A character string identifier of the variety
    name = NULL,
    #' @field parent Estimation parent
    parent = NULL,
    #' @field weather A TPE weather object
    weather = NULL,
    #' @field soil A soil object
    soil = NULL,
    #' @field parameters A dataframe with all parameters used for simulation
    parameters = NULL,
    #' @field observations A list of dataframes of observations
    observations = list(),

    #' @description Create a new environment object
    #' @param name A character string identifier of the environment
    #' @param parent Estimation parent
    #' @param parameters TPE analysis parent
    #' @param weather Weather
    #' @param observations Observations
    #' @return A new `TPEenv` object.
    initialize = function(name="e1", parent, parameters, weather,
                          observations) {
      self$name <- name
      self$parent <- parent
      self$parameters <- parameters
      self$weather <- weather
      self$soil <- TPEsoil$new(self$name, self)
      self$observations <- observations
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
