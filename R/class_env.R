#' R6 Class representing a TPEaseEnv Environment
#'
#' @description
#' The \code{TPEaseEnv} object contains all information of a given environment,
#' defined as a combination of : soil, weather and crop management data
#'
#' @usage
#' \code{TPEaseEnv} are not used as is but are created for specific use cases
#' (estimation or grid simulations)
#'
#' @details
#' The \code{TPEaseEnv} object is a super-class containing common information
#' across the different types of environments
#' (\code{EstimEnv} or \code{GridPoint})
#'
#' @import R6
#' @export
TPEaseEnv <- R6::R6Class("TPEaseEnv",
  public = list(
    #' @field name The \code{TPEaseEnv} identifier
    name = NULL,
    #' @field parent The \code{TPEase} parent object of \code{TPEaseEnv}
    parent = NULL,
    #' @field variety The \code{TPEaseVar} attached to the \code{TPEaseEnv}
    variety = NULL,
    #' @field weather A \code{EnvWeather} object
    weather = NULL,
    #' @field soil A \code{EnvSoil} object
    soil = NULL,
    #' @field cm A \code{EnvCM} object
    cm = NULL,
    #' @field parameters A \code{data.frame} with all crop model environment
    #' parameters
    parameters = NULL,

    #' @description Create a new \code{TPEaseEnv} object
    #' @param name A \code{character} string identifier of the \code{TPEaseEnv}
    #' @param parent A \code{TPEase} parent object of the Environment
    #' @param variety A \code{TPEaseVar} attached to the \code{TPEaseEnv}
    #' @param parameters A \code{data.frame} with all crop model environment
    #' parameters
    #' @param weather A \code{data.frame} with weather data
    #' @param observations A \code{list} of \code{data.frames} of observations
    #' @return A new \code{TPEase} object.
    initialize = function(name="e1", parent, variety, parameters, weather) {
      self$name <- name
      self$parent <- parent
      self$variety <- variety
      self$parameters <- parameters
      self$weather <- weather
      self$soil <- TPEsoil$new(self$name, self)
      self$cm <- TPEcm$new(self$name, self)
    },

    #' @description Set weather
    #' @param val A \code{data.frame} with weather data
    set_weather = function(val) {
      self$weather <- val
    },

    #' @description Set parameters
    #' @param val A \code{data.frame} with parameter values
    set_param = function(val) {
      self$parameters <- val
    }
  )
)
