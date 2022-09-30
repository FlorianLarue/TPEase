#'#' R6 Class Representing a parameter estimation process
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
estimP <- R6::R6Class("estimP",
  public = list(
   #' @field name A character string identifier of estimation
   name = NULL,
   #' @field parent Parent variety
   parent = NULL,
   #' @field environments List of environments to use for estimation
   environments = list(),
   #' @field test Debug
   test = NA,

   #' @description Create a new TPE soil object
   #' @param name A character string identifier of estimation
   #' @param parent Parent variety
   #' @param environments List of environment names
   #' @param eparam Parameters of environment
   #' @param observations Observations of environment
   #' @return A new `TPEsoil` object.
   initialize = function(name="estim1", parent, environments, eparam,
                         observations) {
     self$name <- as.character(name)
     self$parent <- parent

     if(length(environments) > 1 || !is.na(environments)) {
       for(i in 1:length(environments)) {
         self$createEnv(environments[[i]], eparam[i,], observations[[i]])
       }
     }
   },

   #' @description Create environment
   #' @param name Name of environment
   #' @param eparam Parameters of environment
   #' @param observations Observations of environment
   createEnv = function(name, eparam, observations) {
     self$environments <- append(self$environments, TPEenv$new(name, self,
                                                               eparam,
                                                               observations))
   }
  ),

  private = list()
)
