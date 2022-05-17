#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
TPEa <- R6::R6Class("TPEa",
  public = list(
    #' @field name Identifier of the TPE analysis
    name = NULL,
    #' @field model Name of the crop model used for simulations
    model = NULL,

    #' @description Create a new TPE analysis object.
    #' @param name name Identifier of the TPE analysis
    #' @param model model Name of the crop model used for simulations
    #' @return A new `TPEa` object.
    initialize = function(name = NA, model = NA) {
      self$name <- name
      self$model <- model
      self$initMessage()
    },

    #' @description Change crop model
    #' @param val New model name
    #' @examples
    #' TPE_analysis <- TPEa("TPEa1", "STICS")
    #' TPE_analysis$model
    #' TPE_analysis$set_model("Samara")
    #' TPE_analysis$model
    set_model = function(val) {
      self$model <- val
    },

    #' @description Confirm creation of TPE analysis object
    initMessage = function() {
      cat(paste0("TPE analysis ", self$name, " created.\n"))
    }
  ),
  private = list()
)
