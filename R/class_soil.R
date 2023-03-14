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
    #' @field parent Parent environment
    parent = NULL,
    #' @field parameters Soil parameters
    parameters = NULL,
    #' @field soilParam Boolean indicating if soil parameters were extracted
    #' from HC27
    soilParam = FALSE,
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE soil object
    #' @param name A character string identifier of the TPE soil
    #' @param parent Parent environment
    #' @return A new `TPEsoil` object.
    initialize = function(name="soil1", parent=NULL) {
      self$name <- as.character(name)
      parampath <- system.file("extdata", "soil.csv", package="CGMTPE")
      self$parameters <- data.frame(fread(parampath))
      self$parent <- parent
    },

    #' @description Set soil parameters
    #' @param name A \code{character} value (or \code{vector}) of
    #' soil parameter name(s)
    #' @param val A \code{numeric} (or \code{vector}) of soil parameter values
    set_soilParam = function(name, val) {
      for(i in 1:length(name)) {
        if(!is.na(name[i]) & name[i] %in% colnames(self$parameters)) {
          self$parameters[,which(colnames(self$parameters) == name[i])] <-
            val[i]
          self$soilParam <- TRUE
        }
      }
    }
  ),
  private = list()
)
