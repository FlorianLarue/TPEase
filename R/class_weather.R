#'#' R6 Class Representing a TPE weather
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEweather <- R6::R6Class("TPEweather",
  public = list(
    #' @field name A character string identifier of the weather
    name = NULL,
    #' @field wData A dataframe with weather data
    wData = NULL,
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE weather object
    #' @param name A character string identifier of the TPE weather
    #' @return A new `TPEweather` object.
    initialize = function(name="map1") {
      self$name <- as.character(name)
    }
  ),

  private = list(
  )
)
