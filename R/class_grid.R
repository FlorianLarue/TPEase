#'#' R6 Class Representing a simulation grid point
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
TPEgrid <- R6::R6Class("TPEgrid",
  public = list(
    #' @field name Identifier of the grid point
    name = NULL,
    #' @field lat Latitude of the TPE analysis
    lat = NULL,
    #' @field lon Longitude of the TPE analysis
    lon = NULL,
    #' @field weather Samara climate data
    weather = NULL,


    #' @description Create a new TPE grid object.
    #' @param name Identifier of the TPE analysis
    #' @param lat Latitude of grid point
    #' @param lon Longitude of grid point
    #' @return A new `TPEgrid` object.
    initialize = function(name="c11", lat=11.18, lon=-4.3) {
      self$name <- name
      self$lat <- lat
      self$lon <- lon
    },

    #' @description Change weather of grid point
    #' @param weather Weather dataframe
    set_weather = function(weather) {
      self$weather <- weather
    },

    #' @description Generate climate at grid point
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI) {
      climate <- generateClimate(self$lon, self$lat, rcp, year, yearNb, modelNb,
                                 path,pathCLI)
      self$set_weather(climate)
    }
  )
)
