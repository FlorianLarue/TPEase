#' R6 Class representing a TPEaseEnv Environment
#'
#' @description
#' The `TPEaseEnv` object contains all information of a given environment,
#' defined as a combination of : soil, weather and crop management data
#'
#' @import R6
#' @export
TPEaseEnv <- R6::R6Class("TPEaseEnv",
  public = list(
    #' @field name A character string identifier of the TPEaseEnv
    name = NULL,
    #' @field parent A TPEase object parent of the Environment
    parent = NULL,
    #' @field weather A EnvWeather object
    weather = NULL,
    #' @field soil A EnvSoil object
    soil = NULL,
    #' @field cm A EnvCM object
    cm = NULL,
    #' @field parameters A dataframe with all crop model environment parameters
    parameters = NULL,
    #' @field observations A list of dataframes of observations
    observations = list(),

    #' @description Create a new TPEaseEnv object
    #' @param name A character string identifier of the TPEaseEnv
    #' @param parent A TPEase object parent of the Environment
    #' @param parameters A dataframe with all crop model environment parameters
    #' @param weather A dataframe with weather data
    #' @param observations A list of dataframes of observations
    #' @return A new `TPEenv` object.
    initialize = function(name="e1", parent, parameters, weather,
                          observations) {
      self$name <- name
      self$parent <- parent
      self$parameters <- parameters
      self$weather <- weather
      self$soil <- TPEsoil$new(self$name, self)
      self$cm <- TPEcm$new(self$name, self)
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
