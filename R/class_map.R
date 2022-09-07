#'#' R6 Class Representing a TPE map
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEmap <- R6::R6Class("TPEmap",
  public = list(
    #' @field name A character string identifier of the map
    name = NULL,
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE map object
    #' @param name A character string identifier of the TPE map
    #' @return A new `TPEmap` object.
    initialize = function(name="map1") {
      self$name <- as.character(name)
    }
  ),

  private = list(
  )
)
