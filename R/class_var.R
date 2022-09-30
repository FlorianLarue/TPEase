#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEvar <- R6::R6Class("TPEvar",
  public = list(
    #' @field name A character string identifier of the variety
    name = NULL,
    #' @field alt A character string alternate adientifier of the variety
    alt = NULL,
    #' @field parent analysis parent
    parent = NULL,
    #' @field parameters A dataframe with all parameters used for simulation
    parameters = NULL,
    #' @field estimations A list of estimation objects
    estimations = list(),

    #' @description Create a new variety object
    #' @param name A character string identifier of the variety
    #' @param alt A character string alternate identifier of the variety
    #' @param eName Name of initial estimation object
    #' @param environments List of environment names
    #' @param eparam Environment parameters
    #' @param observations List of dataframes of observations
    #' (each entry is for one environment)
    #' @param parent TPE analysis parent
    #' @return A new `TPEvar` object.
    initialize = function(name="v1", alt="G1", eName="estim1",
                          environments=NA, eparam=NA, observations=NA,
                          parent=NA) {
      self$name <- name
      self$alt <- alt
      self$parent <- parent

      self$createEstim(eName, self, environments, eparam, observations)
    },


    #' @description Set parameters
    #' @param val A dataframe with all crop model parameters
    set_param = function(val) {
      self$parameters <- val
    },

    #' @description Set estimated parameters
    #' @param val A vector with parameter values
    #' @param names A vector with estimated parameter names
    update_param = function(val,names) {
      for(p in 1:length(names)) {
        self$parameters[,which(colnames(self$parameters) ==
                                 names[[p]])] <- val[[p]]
      }
    },

    #' @description Create estimation
    #' @param name Name of estimation
    #' @param parent Parent variety
    #' @param environments Name of environments
    #' @param eparam Parameters of environments
    #' @param observations Observations of environments
    createEstim = function(name, parent, environments, eparam, observations) {
      self$estimations <- append(self$estimations,
                                 estimP$new(name, parent, environments, eparam,
                                            observations))
    }
  )
)
