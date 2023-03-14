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
    #' @field name A character string identifier of the crop management
    name = NULL,
    #' @field parameters A dataframe with crop management parameters
    parameters = NULL,

    #' @description Create a new variety object
    #' @param name A character string identifier of the variety
    #' @param parent TPE analysis parent
    #' @return A new `TPEenv` object.
    #' @importFrom data.table fread
    initialize = function(name="e1", parent) {
      self$name <- name
      parampath <- system.file("extdata", "management.csv", package="CGMTPE")
      self$parameters <- data.frame(fread(parampath))
    },

    #' @description Set crop management parameters
    #' @param name A \code{character} value (or \code{vector}) of
    #' crop management parameter name(s)
    #' @param val A \code{numeric} (or \code{vector}) of crop management
    #' parameter values
    set_cmParam = function(name, val) {
      for(i in 1:length(name)) {
        if(!is.na(name[i]) & name[i] %in% colnames(self$parameters)) {
          self$parameters[,which(colnames(self$parameters) == name[i])] <-
            val[i]
        }
      }
    },

    #' @description Set date parameters (starting, ending and sowing date)
    #' @param val A vector of size 3 with starting, ending and sowing dates
    set_dateparam = function(val) {
      self$parameters[,c("startingdate","endingdate","sowing")] <- val
    },

    #' @description Set date latitude and longitude
    #' @param val A vector of size 2 with latitude and longitude of the site
    set_latlon = function(val) {
      self$parameters[,c("wslat","wslong")] <- val
    }
  )
)
