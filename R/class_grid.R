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

    #' @description Create a new TPE grid object.
    #' @param name Identifier of the TPE analysis
    #' @param lat Latitude of grid point
    #' @param lon Longitude of grid point
    #' @return A new `TPEgrid` object.
    initialize = function(name="c11", lat=11.18, lon=-4.3) {
      self$name <- name
      self$lat <- lat
      self$lon <- lon
    }
  )
)
