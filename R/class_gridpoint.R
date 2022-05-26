#'#' R6 Class Representing a simulation grid point
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
gridPoint <- R6::R6Class("gridPoint",
  public = list(
    #' @field name Identifier of the grid point
    name = NULL,
    #' @field lon Longitude of the grid point
    lon = NULL,
    #' @field lat Latitude of the grid point
    lat = NULL,
    #' @field weather Samara climate data
    weather = NULL,
    #' @field result Model simulation
    result = NULL,

    #' @description Create a new TPE grid point object
    #' @param name Identifier of the TPE analysis
    #' @param lat Latitude of grid point
    #' @param lon Longitude of grid point
    #' @return A new `TPEgrid` object.
    initialize = function(name="11", lon=-4.3, lat=11.18) {
      self$name <- name
      self$lon <- lon
      self$lat <- lat
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
    },

    #' @description Run simulation on grid point
    #' @param param Parameter values
    #' @import stringr
    runSimulation = function(param) {
      sy <- strsplit(rsamara::toStringDateCalcc(param$startingdate),
                     split="/")[[1]][[3]]
      ey <- strsplit(rsamara::toStringDateCalcc(param$endingdate),
                     split="/")[[1]][[3]]
      sm <- strsplit(rsamara::toStringDateCalcc(param$startingdate),
                     split="/")[[1]][[2]]
      em <- strsplit(rsamara::toStringDateCalcc(param$endingdate),
                     split="/")[[1]][[2]]
      simWeather <- self$weather[which(stringr::str_split_fixed(
        self$weather$weatherdate, "/",3)[,3] %in% c(sy,ey)),]
      simWeather <- simWeather[which(stringr::str_split_fixed(
        simWeather$weatherdate, "/",3)[,2] >= sm),]
      simWeather <- simWeather[which(stringr::str_split_fixed(
        simWeather$weatherdate, "/",3)[,2] <= em),]

      rsamara::init_sim_idx_simple(as.numeric(self$name), param, simWeather)
      res <- rsamara::run_sim_idx(as.numeric(self$name))
      self$result <- res
    }
  )
)
