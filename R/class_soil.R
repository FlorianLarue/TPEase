#'#' R6 Class Representing a TPE soil
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEsoil <- R6::R6Class("TPEsoil",
  public = list(
    #' @field name A character string identifier of the weather
    name = NULL,
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE soil object
    #' @param name A character string identifier of the TPE soil
    #' @return A new `TPEsoil` object.
    initialize = function(name="map1") {
      self$name <- as.character(name)
    }
  ),

  private = list(
  )
)
