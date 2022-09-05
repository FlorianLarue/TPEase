#'#' R6 Class Representing a crop managment
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEcm <- R6::R6Class("TPEcm",
  public = list(
    #' @field name A character string identifier of the variety
    name = NULL,
    #' @field soilParam soil parameters of grid point
    soilParam = NULL,
    #' @field dateParam sowing date of grid point
    dateParam = NULL,

    #' @description Create a new variety object
    #' @param name A character string identifier of the variety
    #' @param parent TPE analysis parent
    #' @return A new `TPEenv` object.
    initialize = function(name="e1", parent) {
      self$name <- name
    }
  )
)
